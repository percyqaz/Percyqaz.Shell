namespace Percyqaz.Shell

module Check =

    open Tree

    module private Patterns = 
        let (|String|_|) = function Type.String | Type.Any -> Some String | _ -> None
        let (|Number|_|) = function Type.Number | Type.Any -> Some Number | _ -> None
        let (|Bool|_|) = function Type.Bool | Type.Any -> Some Bool | _ -> None
        let (|Null|_|) = function Type.Null | Type.Any -> Some Null | _ -> None
        let (|Object|_|) = function Type.Object xs -> Some xs | Type.Any -> Some Map.empty | _ -> None
        let (|Array|_|) = function Type.Array ty -> Some ty | Type.Any -> Some Type.Any | _ -> None

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
    let rec infer_type (ex: Expr) (ctx: Context) : Type * ExprC =
        match ex with
        | Expr.String s -> Type.String, ExprC.String s
        | Expr.Number n -> Type.Number, ExprC.Number n
        | Expr.Bool b -> Type.Bool, ExprC.Bool b
        | Expr.Null -> Type.Null, ExprC.Null
        | Expr.Object xs ->
            let mutable new_xs = Map.empty
            let mutable ts = Map.empty
            for (KeyValue(prop, propEx)) in xs do
                let ty, new_ex = infer_type propEx ctx
                ts <- Map.add prop ty ts
                new_xs <- Map.add prop new_ex new_xs
            Type.Object ts, ExprC.Object new_xs
        | Expr.Array xs ->
            match xs with
            | [] -> Type.Array Type.Any, ExprC.Array []
            | head :: rest ->
                let ty, headChecked = infer_type head ctx
                let ys = headChecked :: List.map (fun item -> type_check_expr ty item ctx) rest
                Type.Array ty, ExprC.Array ys
        | Expr.Pipeline (head, rest) ->
            let headTy, headChecked = infer_type head ctx
            let restTy, restChecked = infer_type rest (ctx.WithPipelineType headTy)
            restTy, ExprC.Pipeline(headChecked, restChecked)
        | Expr.Pipeline_Variable ->
            match Map.tryFind "" ctx.Variables with
            | Some (ty, _) -> ty, ExprC.Pipeline_Variable
            | None -> failwith "The pipeline variable does not exist in this context"
        | Expr.Variable v ->
            match Map.tryFind v ctx.Variables with
            | Some (ty, _) -> ty, ExprC.Variable v
            | None -> failwithf "Unrecognised variable: '%s'" v
        | Expr.Subscript (body, sub) ->
            match infer_type body ctx with
            | Type.Array arrtype, bodyChecked -> 
                match infer_type sub ctx with
                | Type.Number, subChecked -> arrtype, ExprC.Subscript(bodyChecked, subChecked)
                | _ -> failwith "Subscript indices must be numbers"
            | _ -> failwith "Subscripting a non-array"
        | Expr.Property (body, prop) ->
            match infer_type body ctx with
            | Type.Object props, checkedBody ->
                match Map.tryFind prop props with
                | Some ptype -> ptype, ExprC.Property(checkedBody, prop)
                | None -> failwithf "Unrecognised property: '%s'" prop
            | _ -> failwith "Accessing property of a non-object"
        | Expr.Command req ->
            let reqChecked = type_check_command Type.Any req ctx
            ctx.Commands.[req.Name].Signature.ReturnType, ExprC.Command reqChecked
        | Expr.Cond (arms, basecase) ->
            let ty, baseChecked = infer_type basecase ctx
            let checkedArms =
                List.map
                    ( fun (cond, armex) ->
                        type_check_expr ty ex ctx,
                        type_check_expr Type.Bool cond ctx
                    ) arms
            ty, ExprC.Cond (checkedArms, baseChecked)
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
            ty, ExprC.Block (stmtsChecked, exprChecked)
        | Expr.Lambda _ -> failwith "nyi"
        
    /// returns adjusted expression (for casts and stuff) or throws exception
    and type_check_expr (t: Type) (ex: Expr) (ctx: Context) : ExprC =
        match t, ex with
        | Patterns.String, Expr.String s -> ExprC.String s
        | Patterns.Number, Expr.Number n -> ExprC.Number n
        | Patterns.Bool, Expr.Bool b -> ExprC.Bool b
        | Patterns.Null, Expr.Null -> ExprC.Null
        | Patterns.Object ms, Expr.Object xs ->
            let mutable new_xs = Map.empty
            // Check all props according to type
            Map.iter ( fun prop propType ->
                if xs.ContainsKey prop then
                    new_xs <- Map.add prop (type_check_expr propType xs.[prop] ctx) new_xs
                else failwithf "Missing property '%s': Expected a %O" prop propType
            ) ms
            // Check all extra props
            Map.iter ( fun prop propEx ->
                if not (new_xs.ContainsKey prop) then
                    new_xs <- Map.add prop (type_check_expr Type.Any propEx ctx) new_xs
            ) xs
            ExprC.Object new_xs

        | Patterns.Array ty, Expr.Array xs ->
            List.map (fun item -> type_check_expr ty item ctx) xs |> ExprC.Array
        | _, Expr.Pipeline_Variable ->
            match Map.tryFind "" ctx.Variables with
            | Some (ty, _) -> type_unify t ty; ExprC.Pipeline_Variable
            | None -> failwith "The pipeline variable does not exist in this context"
        | _, Expr.Variable v ->
            match Map.tryFind v ctx.Variables with
            | Some (ty, _) -> type_unify t ty; ExprC.Variable v
            | None -> failwithf "Unrecognised variable: '%s'" v
        | ty, Expr.Pipeline (head, rest) ->
            let headTy, headChecked = infer_type head ctx
            ExprC.Pipeline (
                headChecked,
                type_check_expr ty rest (ctx.WithPipelineType headTy)
            )
        | ty, Expr.Subscript (ex, sub) ->
            ExprC.Subscript (
                type_check_expr (Type.Array ty) ex ctx,
                type_check_expr Type.Number sub ctx
            )
        | ty, Expr.Property (ex, prop) ->
            ExprC.Property (
                type_check_expr (Type.Object (Map.ofList [(prop, ty)])) ex ctx,
                prop
            )
        | ty, Expr.Command reqex -> 
            ExprC.Command (type_check_command ty reqex ctx)
        | ty, Expr.Cond (arms, basecase) ->
            ExprC.Cond (
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
            ExprC.Block (stmtsChecked, exprChecked)
        | ty, Expr.Lambda _ -> failwith "nyi"
        | _, _ -> failwithf "Expected a %O but got: %O" t ex
        
    /// returns adjusted request (for casts and stuff) or throws exception
    and type_check_command (t: Type) (rx: CommandRequest) (ctx: Context) : CommandRequestC =
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
    and type_check_stmt (stmt: Statement) (ctx: Context) : StatementC * Context =
        match stmt with
        | Statement.Declare (name, ty, expr) ->
            let ty, exchecked =
                match ty with
                | None -> infer_type expr ctx
                | Some expectedType -> expectedType, type_check_expr expectedType expr ctx
            StatementC.Declare (name, ty, exchecked),
            ctx.WithVarType(name, ty)
        | Statement.Eval expr ->
            StatementC.Eval (type_check_expr Type.Any expr ctx), ctx
        | Statement.Help h -> StatementC.Help h, ctx