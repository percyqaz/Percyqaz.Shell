namespace Percyqaz.Shell

module Check =

    open Tree

    // actually bad name, checks if current type matches target
    // unifying will be a different behaviour to join two types
    let rec type_unify (target: Type) (current: Type) : unit =
        match target, current with
        | Type.Any, _ -> ()
        | Type.String, Type.String -> ()
        | Type.Number, Type.Number -> ()
        | Type.Bool, Type.Bool -> ()
        | Type.Null, Type.Null -> ()
        | Type.Object xs, Type.Object ys ->
            for (KeyValue(prop, propType)) in xs do
                if ys.ContainsKey(prop) then type_unify propType ys.[prop]
                else failwithf "Target type requires property '%s' : %O" prop propType
        | Type.Array t1, Type.Array t2 -> type_unify t1 t2
        | _, _ -> failwithf "Cannot unify actual type %O with expected type %O" current target

    /// returns inferred type + adjusted expression, or throws exception
    let rec infer_type (ex: Expr) (ctx: Context) : Type * Expr =
        match ex with
        | Expr.String _ -> Type.String, ex
        | Expr.Number _ -> Type.Number, ex
        | Expr.Bool _ -> Type.Bool, ex
        | Expr.Null -> Type.Null, ex
        | Expr.Object xs ->
            let mutable new_xs = Map.empty
            let mutable ts = Map.empty
            for (KeyValue(prop, propEx)) in xs do
                let ty, new_ex = infer_type propEx ctx
                ts <- Map.add prop ty ts
                new_xs <- Map.add prop new_ex new_xs
            Type.Object ts, Expr.Object new_xs
        | Expr.Array xs -> Type.Array Type.Any, ex //nyi
        
        | Expr.Pipeline (head, rest) ->
            let headTy, headChecked = infer_type head ctx
            let restTy, restChecked = infer_type rest (ctx.WithPipelineType headTy)
            restTy, Expr.Pipeline(headChecked, restChecked)
        | Expr.Pipeline_Variable ->
            match Map.tryFind "" ctx.Variables with
            | Some (ty, _) -> ty, ex
            | None -> failwith "The pipeline variable does not exist in this context"
        | Expr.Variable v ->
            match Map.tryFind v ctx.Variables with
            | Some (ty, _) -> ty, ex
            | None -> failwithf "Unrecognised variable: '%s'" v
        | Expr.Subscript (body, sub) ->
            match infer_type body ctx with
            | Type.Array arrtype, bodyChecked -> 
                match infer_type sub ctx with
                | Type.Number, subChecked -> arrtype, Expr.Subscript(bodyChecked, subChecked)
                | _ -> failwith "Subscript indices must be numbers"
            | _ -> failwith "Subscripting a non-array"
        | Expr.Property (body, prop) ->
            match infer_type body ctx with
            | Type.Object props, checkedBody ->
                match Map.tryFind prop props with
                | Some ptype -> ptype, Expr.Property(checkedBody, prop)
                | None -> failwithf "Unrecognised property: '%s'" prop
            | _ -> failwith "Accessing property of a non-object"
        | Expr.Command req ->
            let reqChecked = type_check_command Type.Any req ctx
            ctx.Commands.[req.Name].Signature.ReturnType, Expr.Command reqChecked
        | Expr.Cond (arms, basecase) ->
            let ty, baseChecked = infer_type basecase ctx
            let checkedArms =
                List.map
                    ( fun (cond, armex) ->
                        type_check_expr ty ex ctx,
                        type_check_expr Type.Bool cond ctx
                    ) arms
            ty, Expr.Cond (checkedArms, baseChecked)
        | Expr.Try (expr, iferror) -> failwith "nyi"
        | Expr.Block (stmts, expr) ->
            let rec loop stmts ctx =
                match stmts with
                | [] -> [], ctx
                | stmt :: xs ->
                    let chk, ctx = type_check_stmt stmt ctx
                    let ys, res = loop xs ctx
                    chk :: ys, res
            let stmtsChecked, resultingCtx = loop stmts ctx
            let ty, exprChecked = infer_type expr resultingCtx
            ty, Expr.Block (stmtsChecked, exprChecked)
        | Expr.Lambda _ -> failwith "nyi"
        
    /// returns adjusted expression (for casts and stuff) or throws exception
    and type_check_expr (t: Type) (ex: Expr) (ctx: Context) : Expr =
        match t, ex with
        | Type.String, Expr.String _ -> ex
        | Type.Number, Expr.Number _ -> ex
        | Type.Bool, Expr.Bool _ -> ex
        | Type.Null, Expr.Null -> ex
        | Type.Object ms, Expr.Object xs ->
            let mutable new_xs = xs
            for (KeyValue(prop, propType)) in ms do
                if xs.ContainsKey(prop) then new_xs <- Map.add prop (type_check_expr propType xs.[prop] ctx) new_xs
                else failwithf "Missing property '%s': Expected a %O" prop propType
            Expr.Object new_xs
        | Type.Array ty, Expr.Array xs ->
            List.map (fun item -> type_check_expr ty item ctx) xs |> Expr.Array
        | _, Expr.Pipeline_Variable ->
            match Map.tryFind "" ctx.Variables with
            | Some (ty, _) -> type_unify t ty; ex
            | None -> failwith "The pipeline variable does not exist in this context"
        | _, Expr.Variable v ->
            match Map.tryFind v ctx.Variables with
            | Some (ty, _) -> type_unify t ty; ex
            | None -> failwithf "Unrecognised variable: '%s'" v
        | ty, Expr.Pipeline (head, rest) ->
            let headTy, headChecked = infer_type head ctx
            Expr.Pipeline (
                headChecked,
                type_check_expr ty rest (ctx.WithPipelineType headTy)
            )
        | ty, Expr.Subscript (ex, sub) ->
            Expr.Subscript (
                type_check_expr (Type.Array ty) ex ctx,
                type_check_expr Type.Number sub ctx
            )
        | ty, Expr.Property (ex, prop) ->
            Expr.Property (
                type_check_expr (Type.Object (Map.ofList [(prop, ty)])) ex ctx,
                prop
            )
        | ty, Expr.Command reqex -> 
            Expr.Command (type_check_command ty reqex ctx)
        | ty, Expr.Cond (arms, basecase) ->
            Expr.Cond (
                List.map 
                    ( fun (cond, ex) ->
                        type_check_expr Type.Bool cond ctx,
                        type_check_expr ty ex ctx
                    ) arms,
                type_check_expr ty basecase ctx
            )
        | ty, Expr.Try (expr, iferror) -> failwith "nyi"
        | ty, Expr.Block (stmts, expr) ->
            let rec loop stmts ctx =
                match stmts with
                | [] -> [], ctx
                | stmt :: xs ->
                    let chk, ctx = type_check_stmt stmt ctx
                    let ys, res = loop xs ctx
                    chk :: ys, res
            let stmtsChecked, resultingCtx = loop stmts ctx
            let exprChecked = type_check_expr ty expr resultingCtx
            Expr.Block (stmtsChecked, exprChecked)
        | ty, Expr.Lambda _ -> failwith "nyi"
        | Type.Any, _ -> ex
        | _, _ -> failwithf "Expected a %O but got: %O" t ex
        
    /// returns adjusted request (for casts and stuff) or throws exception
    and type_check_command (t: Type) (rx: CommandRequest) (ctx: Context) : CommandRequest =
        if not (Map.containsKey rx.Name ctx.Commands) then failwithf "Unrecognised command '%s'" rx.Name
        let cmd = ctx.Commands[rx.Name]

        let mutable args = rx.Args

        let mutable checked_args = []

        for (name, ty) in cmd.Signature.Args do
            match args with
            | x :: xs ->
                checked_args <- type_check_expr ty x ctx :: checked_args
                args <- xs
            | [] -> failwithf "Missing argument '%s' : %O" name ty

        for (name, ty) in cmd.Signature.OptArgs do
            match args with
            | x :: xs ->
                checked_args <- type_check_expr ty x ctx :: checked_args
                args <- xs
            | [] -> ()

        if args <> [] then
            failwithf "Unexpected argument(s): %s" (List.map (sprintf "%O") args |> String.concat ", ")

        let checked_flags =
            Map.map
                ( fun name v ->
                    if cmd.Signature.Flags.ContainsKey name then
                        type_check_expr cmd.Signature.Flags.[name] v ctx
                    else failwithf "Unrecognised flag '%s'" name
                ) rx.Flags

        type_unify t cmd.Signature.ReturnType

        {
            Name = rx.Name
            Args = List.rev checked_args
            Flags = checked_flags
        }
        
    /// returns state of the context after this would be executed, or list of errors
    and type_check_stmt (stmt: Statement) (ctx: Context) : Statement * Context =
        match stmt with
        | Statement.Declare (name, ty, expr) ->
            let ty, exchecked =
                match ty with
                | None -> infer_type expr ctx
                | Some expectedType -> expectedType, type_check_expr expectedType expr ctx
            Statement.Declare (name, Some ty, exchecked), // Major issue: we now know this is not None but not known by type system
            ctx.WithVarType(name, ty)
        | Statement.Eval expr ->
            Statement.Eval (type_check_expr Type.Any expr ctx), ctx
        | Statement.Help _ -> stmt, ctx