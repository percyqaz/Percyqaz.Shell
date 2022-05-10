namespace Percyqaz.Shell

module Runtime =
    
    open Tree

    let show_command (ctx: Context) (name: string) (func: Func) =
        ctx.WriteLine(sprintf "Showing help for '%s':\n" name)
    
        ctx.Write("usage: " + name)
        for arg in func.Binds do
            ctx.Write(sprintf " <%s>" arg)
        ctx.WriteLine("\n" + func.Desc)

    let private error ex message = failwithf "%O : %s" ex message

    let rec do_stmt (stmt: Stmt) (ctx: Context) (echo: bool) : Context =
        match stmt with
        | Stmt.Decl (name, ex) -> ctx.WithVar(name, eval_expr ex ctx)
        | Stmt.Eval ex ->
            match eval_expr ex ctx with
            | Val.Nil -> ()
            | x -> if echo then sprintf "%O" x |> ctx.WriteLine
            ctx
        | Stmt.Help_ModuleCmd (mname, id) ->
            match ctx.Modules.TryFind mname with
            | Some m ->
                match m.Vars.TryFind id with
                | Some (Val.Func f) -> show_command ctx (mname + "::" + id) f
                | Some (_) -> failwith "This is a variable, not a command"
                | None -> failwith "No such command"
            | None -> failwith "No such module"
            ctx
        | Stmt.Help_ModuleOrCmd id ->
            let mutable command = true
            match ctx.Vars.TryFind id with
            | Some (Val.Func f) -> show_command ctx id f
            | Some (_) -> failwith "This is a variable, not a command"
            | None -> command <- false
            match ctx.Modules.TryFind id with
            | Some m ->
                let commands = m.Vars |> Map.filter (fun _ -> function Val.Func _ -> true | _ -> false) |> Map.keys
                ctx.WriteLine(sprintf "Available commands in module '%s': %s" id (String.concat ", " commands))
            | None -> if not command then failwith "No such command or module"
            ctx
        | Stmt.Help_All ->
            let commands = ctx.Vars |> Map.filter (fun _ -> function Val.Func _ -> true | _ -> false) |> Map.keys
            ctx.WriteLine(sprintf "Available commands: %s" (String.concat ", " commands))
            ctx

    and eval_expr (ex: Expr) (ctx: Context) : Val =
        match ex with
        | Expr.Str lit -> Val.Str lit
        | Expr.StrInterp frags ->
            List.map (function StrFrag.Str s -> s | StrFrag.Ex x -> (eval_expr x ctx).ToString()) frags
            |> String.concat ""
            |> Val.Str
        | Expr.Num n -> Val.Num n
        | Expr.Bool b -> Val.Bool b
        | Expr.Nil -> Val.Nil
        | Expr.Obj xs -> Map.map (fun _ ex -> eval_expr ex ctx) xs |> Val.Obj
        | Expr.Arr xs -> List.map (fun ex -> eval_expr ex ctx) xs |> Val.Arr
        | Expr.Func (binds, body) -> construct_lambda binds body ctx
        
        | Expr.Monop (op, ex) -> eval_monop op ex ctx
        | Expr.Binop (op, left, right) -> eval_binop op left right ctx
        | Expr.Pipevar ->
            match ctx.Vars.TryFind "" with
            | None -> error ex "The pipeline variable does not exist in this context"
            | Some v -> v
        | Expr.Var id ->
            match ctx.Vars.TryFind id with
            | None -> error ex "No such variable"
            | Some v -> v
        | Expr.ModVar (m, id) ->
            match ctx.Modules.TryFind m with
            | None -> error ex "No such module"
            | Some m ->
                match Map.tryFind id m.Vars with
                | None -> error ex "No such module variable"
                | Some v -> v
        | Expr.Sub (main, sub) ->
            match eval_expr main ctx with
            | Val.Arr xs -> 
                match eval_monop Monop.ROUND sub ctx with
                | Val.Num n -> try xs.[int n] with _ -> error sub "Index out of bounds"
                | _ -> error sub "Expected an array index"
            | Val.Obj xs ->
                match eval_monop Monop.STR sub ctx with
                | Val.Str prop ->
                    match xs.TryFind prop with
                    | None -> error ex "Object has no such property"
                    | Some v -> v
                | _ -> error sub "Expected a property identifier"
            | _ -> error ex "Value is not subscriptable"
        | Expr.Prop (main, prop) ->
            match eval_expr main ctx with
            | Val.Obj xs ->
                match xs.TryFind prop with
                | None -> error ex "Object has no such property"
                | Some v -> v
            | _ -> error ex "Expected an object"
        | Expr.Cond (arms, basecase) ->
            let rec loop arms =
                match arms with 
                | (cond, ex) :: arms -> 
                    match eval_monop Monop.TRUTH cond ctx with
                    | Val.Bool true ->
                        eval_expr ex ctx
                    | _ -> loop arms
                | [] -> eval_expr basecase ctx
            loop arms

        | Expr.Block (stmts, ex) ->
            let rec loop stmts ctx =
                match stmts with
                | [] -> ctx
                | stmt :: xs -> loop xs (do_stmt stmt ctx false)
            eval_expr ex (loop stmts ctx)
        | Expr.Cmd (id, args) ->
            match ctx.Vars.TryFind id with
            | Some (Val.Func f) -> f.Impl (List.map (fun ex -> eval_expr ex ctx) args)
            | Some _ -> error ex "Value is not callable"
            | None -> error ex "No such command"
        | Expr.ModCmd (m, id, args) ->
            match ctx.Modules.TryFind m with
            | None -> error ex "No such module"
            | Some m ->
                match Map.tryFind id m.Vars with
                | Some (Val.Func f) -> f.Impl (List.map (fun ex -> eval_expr ex ctx) args)
                | Some _ -> error ex "Module value is not callable"
                | None -> error ex "No such module command"
        | Expr.VarCall (main, args) ->
            match eval_expr main ctx with
            | Val.Func f -> f.Impl (List.map (fun ex -> eval_expr ex ctx) args)
            | _ -> error ex "Value is not callable"

    and eval_monop (op: Monop) (ex: Expr) (ctx: Context) : Val =
        match op with
        | Monop.ECHO ->
            let v = eval_expr ex ctx in ctx.WriteLine (sprintf "%O" v); v
        | Monop.STR -> Val.Str (sprintf "%O" (eval_expr ex ctx))
        | Monop.TRUTH ->
            match eval_expr ex ctx with
            | Val.Str s -> s <> ""
            | Val.Num n -> n <> 0
            | Val.Bool b -> b
            | Val.Nil -> false
            | Val.Arr xs -> not xs.IsEmpty
            | Val.Obj xs -> error ex "Cannot cast object to bool"
            | Val.Func _ -> error ex "Cannot cast function to bool"
            |> Val.Bool
        | Monop.NOT ->
            match eval_expr ex ctx with
            | Val.Bool b -> Val.Bool (not b)
            | _ -> error ex "Expected a bool"
        | Monop.NEG ->
            match eval_expr ex ctx with
            | Val.Num n -> Val.Num (-n)
            | _ -> error ex "Expected a number"
        | Monop.ROUND ->
            match eval_expr ex ctx with
            | Val.Num n -> Val.Num (System.Math.Round n)
            | _ -> error ex "Expected a number"

    and eval_binop (op: Binop) (left: Expr) (right: Expr) (ctx: Context) : Val =
        
        let numop (f: float -> float -> float) =
            match (eval_expr left ctx, eval_expr right ctx) with
            | Val.Num a, Val.Num b -> Val.Num (f a b)
            | Val.Num _, _ -> error right "Expected a number"
            | _ -> error left "Expected a number"

        match op with
        | Binop.OR ->
            match eval_expr left ctx with
            | Val.Bool true -> Val.Bool true
            | Val.Bool false ->
                match eval_expr right ctx with
                | Val.Bool b -> Val.Bool b
                | _ -> error right "Expected a bool"
            | _ -> error left "Expected a bool"
        | Binop.AND ->
            match eval_expr left ctx with
            | Val.Bool false -> Val.Bool false
            | Val.Bool true ->
                match eval_expr right ctx with
                | Val.Bool b -> Val.Bool b
                | _ -> error right "Expected a bool"
            | _ -> error left "Expected a bool"

        | Binop.PIPE -> eval_expr right (ctx.WithPipeVar(eval_expr left ctx))
        | Binop.ADD -> numop (+)
        | Binop.SUB -> numop (-)
        | Binop.MUL -> numop (*)
        | Binop.DIV -> numop (/)

    and construct_lambda (binds: string list) (body: Expr) (ctx: Context) : Val =
        {
            Binds = binds
            Desc = "Anonymous function"
            Impl =
                let c = binds.Length
                fun xs ->
                    if xs.Length <> c then
                        failwithf "Expected %i arguments" c
                    else
                        let mutable ctx = ctx
                        for (name, v) in List.zip binds xs do
                            ctx <- ctx.WithVar(name, v)
                        eval_expr body ctx
        } |> Val.Func