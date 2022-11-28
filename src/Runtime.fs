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

    module Coerce =
        
        let string (value: Val) : string = 
            match value with
            | Val.Str s -> s
            | _ -> value.ToString()

        let bool (value: Val) : bool =
            match value with
            | Val.Str s -> s <> ""
            | Val.Num n -> n <> 0
            | Val.Bool b -> b
            | Val.Nil -> false
            | Val.Arr xs -> not xs.IsEmpty
            | Val.Obj _ -> error value "Cannot cast object to bool"
            | Val.Func _ -> error value "Cannot cast function to bool"

        open System
        open System.Globalization

        let num (value: Val) : float =
            match value with
            | Val.Str s -> 
                let ok, r = Double.TryParse(s, NumberStyles.AllowLeadingSign ||| NumberStyles.AllowDecimalPoint, CultureInfo.InvariantCulture)
                if ok then r else error value "Cannot cast value to num"
            | Val.Num n -> n
            | _ -> error value "Cannot cast value to num"

        let nil (_: Val) : unit = ()

    let rec do_stmt (stmt: Stmt) (ctx: Context) (echo: bool) : Context =
        match stmt with
        | Stmt.Decl (name, ex) -> ctx.WithVar(name, eval_expr ex ctx)
        | Stmt.Eval ex ->
            match eval_expr ex ctx with
            | Val.Nil -> ()
            | Val.Func f when f.Binds.IsEmpty ->
                let x = f.Impl []
                match x with
                | Val.Nil -> ()
                | _ -> if echo then Coerce.string x |> ctx.WriteLine
            | x -> if echo then Coerce.string x |> ctx.WriteLine
            ctx
        | Stmt.Help_ModuleCmd (mname, id) ->
            match ctx.Modules.TryFind mname with
            | Some m ->
                match m.Vars.TryFind id with
                | Some (Val.Func f) -> show_command ctx (mname + ":" + id) f
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
            let modules = ctx.Modules |> Map.keys
            ctx.WriteLine(sprintf "Available commands: %s" (String.concat ", " commands))
            ctx.WriteLine(sprintf "Available modules: %s" (String.concat ", " modules))
            ctx

    and eval_arg (arg: Arg) (ctx: Context) : Val =
        match arg with
        | Arg.Expr ex -> eval_expr ex ctx
        | Arg.Pure s -> Val.Str s

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
                | Val.Str s -> try xs.[Val.Str s |> Coerce.num |> int] with _ -> error sub "Index out of bounds"
                | _ -> error sub "Expected an array index"
            | Val.Obj xs ->
                match xs.TryFind (Coerce.string (eval_expr sub ctx)) with
                | None -> error ex "Object has no such property"
                | Some v -> v
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
                    if 
                        eval_expr cond ctx
                        |> Coerce.bool
                    then eval_expr ex ctx
                    else loop arms
                | [] -> eval_expr basecase ctx
            loop arms

        | Expr.Block (stmts, ex) ->
            let rec loop stmts ctx =
                match stmts with
                | [] -> ctx
                | stmt :: xs -> loop xs (do_stmt stmt ctx false)
            eval_expr ex (loop stmts ctx)
        | Expr.App (ex, args) ->
            match eval_expr ex ctx with
            | Val.Func f -> f.Impl (List.map (fun ex -> eval_arg ex ctx) args)
            | _ -> error ex "Value is not callable"
        | Expr.Call (main, args) ->
            match eval_expr main ctx with
            | Val.Func f -> f.Impl (List.map (fun ex -> eval_expr ex ctx) args)
            | _ -> error ex "Value is not callable"

    and eval_monop (op: Monop) (ex: Expr) (ctx: Context) : Val =
        match op with
        | Monop.NOT ->
            eval_expr ex ctx
            |> Coerce.bool
            |> not
            |> Val.Bool
        | Monop.NEG ->
            eval_expr ex ctx
            |> Coerce.num
            |> fun x -> -x
            |> Val.Num
        | Monop.ROUND ->
            eval_expr ex ctx
            |> Coerce.num
            |> System.Math.Round
            |> Val.Num
        | Monop.LEN ->
            match eval_expr ex ctx with
            | Val.Arr xs -> xs.Length |> float |> Val.Num
            | _ -> error ex "Expected an array"

    and eval_binop (op: Binop) (left: Expr) (right: Expr) (ctx: Context) : Val =
        
        let numop (f: float -> float -> float) =
            match (eval_expr left ctx, eval_expr right ctx) with
            | Val.Num a, Val.Num b -> Val.Num (f a b)
            | a, b -> Val.Num (f (Coerce.num a) (Coerce.num b))

        match op with
        | Binop.OR ->
            if 
                eval_expr left ctx
                |> Coerce.bool
            then Val.Bool true
            else
                eval_expr right ctx
                |> Coerce.bool
                |> Val.Bool
        | Binop.AND ->
            if 
                eval_expr left ctx
                |> Coerce.bool
            then 
                eval_expr right ctx
                |> Coerce.bool
                |> Val.Bool
            else Val.Bool false

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