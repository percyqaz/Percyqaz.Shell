namespace Percyqaz.Shell

module Check =

    open Tree

    [<RequireQualifiedAccess>]
    type TypeError =
        | Leaf of msg: string
        | Node of name: string * TypeError list // list should be nonempty
    type Res = TypeError list

    module Err =
        let wrap name es = TypeError.Node (name, es)

        let wraps name es = Error [wrap name es]

        let pick name errs =
            match errs with
            | [] -> None
            | xs -> Some (wrap name xs)

        let pickf namefunc errfunc o =
            match errfunc o with
            | [] -> None
            | xs -> Some (wrap (namefunc o) xs)

        let one s = [TypeError.Leaf s]

        let format es : string list =
            let rec manyLoop depth es output : string list =
                let rec oneLoop depth e output : string list =
                    match e with
                    | TypeError.Leaf m -> sprintf "%s%s" (String.replicate depth " ") m :: output
                    | TypeError.Node (name, children) ->
                        sprintf "%s@%s:" (String.replicate depth " ") name :: manyLoop (depth + 1) children output
                match es with
                | [] -> output
                | x :: xs -> oneLoop depth x (manyLoop depth xs output)
            manyLoop 0 es []

    // actually bad name, checks if current type matches target
    // unifying will be a different behaviour to join two types
    let rec type_unify (target: Type) (current: Type) : Res =
        match target, current with
        | Type.Any, _ -> []
        | Type.String, Type.String -> []
        | Type.Number, Type.Number -> []
        | Type.Bool, Type.Bool -> []
        | Type.Null, Type.Null -> []
        | Type.Object xs, Type.Object ys ->
            Seq.choose (
                Err.pickf
                    ( fun (KeyValue(prop, propType)) -> sprintf "Property '%s'" prop )
                    ( fun (KeyValue(prop, propType)) ->
                        if ys.ContainsKey(prop) then type_unify propType ys.[prop]
                        else Err.one (sprintf "Target type requires property '%s' : %O" prop propType)
                    )
            ) xs
            |> List.ofSeq
        | Type.Array t1, Type.Array t2 -> List.choose id [Err.pick "Array type" (type_unify t1 t2)]
        | Type.Closure, _ -> Err.one "nyi"
        | _, _ -> Err.one (sprintf "Cannot unify actual type %O with expected type %O" current target)

    let rec infer_type (ex: Expr) (ctx: Context) : Result<Type, Res> =
        match ex with
        | Expr.String _ -> Ok Type.String
        | Expr.Number _ -> Ok Type.Number
        | Expr.Bool _ -> Ok Type.Bool
        | Expr.Null -> Ok Type.Null
        | Expr.Object xs ->
            let mutable ts = Map.empty
            Seq.choose (
                Err.pickf
                    ( fun (KeyValue(prop, propEx)) -> sprintf "Property '%s'" prop )
                    ( fun (KeyValue(prop, propEx)) ->
                        match infer_type xs.[prop] ctx with
                        | Ok ptype -> ts <- Map.add prop ptype ts; []
                        | Error xs -> xs
                    )
            ) xs
            |> List.ofSeq
            |> function [] -> Ok (Type.Object ts) | xs -> Error xs
        | Expr.Array xs -> Ok (Type.Array (Type.Any)) // nyi
        | Expr.Closure _ -> Error (Err.one "nyi")

        | Expr.Piped_Input ->
            match Map.tryFind "" ctx.Variables with
            | Some (ty, _) -> Ok ty
            | None -> Error (Err.one "The pipeline variable does not exist in this context")
        | Expr.Variable v ->
            match Map.tryFind v ctx.Variables with
            | Some (ty, _) -> Ok ty
            | None -> Error (Err.one (sprintf "Unrecognised variable: '%s'" v))
        | Expr.Subscript (ex, sub) ->
            match infer_type ex ctx with
            | Ok (Type.Array arrtype) -> 
                match infer_type sub ctx with
                | Ok Type.Number -> Ok arrtype
                | Ok _ -> Error (Err.one "Indicies must be numbers")
                | Error xs -> Err.wraps "Subscript index" xs
            | Ok _ -> Error (Err.one "Subscripting a non-array")
            | Error xs -> Err.wraps "Subscriptand" xs
        | Expr.Property (ex, prop) ->
            match infer_type ex ctx with
            | Ok (Type.Object props) ->
                match Map.tryFind prop props with
                | Some ptype -> Ok ptype
                | None -> Error (Err.one (sprintf "Unrecognised property: '%s'" prop))
            | Ok _ -> Error (Err.one "Accessing property of a non-object")
            | Error xs -> Err.wraps "Object expression" xs
        | Expr.Evaluate_Command reqex ->
            match type_check_reqex Type.Any reqex ctx with
            | [] -> Ok ctx.Commands.[reqex.Name].Signature.ReturnType
            | xs -> Err.wraps "Command expression" xs
        | Expr.Cond (cond, iftrue, iffalse) ->
            match infer_type cond ctx with
            | Ok Type.Bool ->
                match infer_type iftrue ctx with
                | Ok ty ->
                    match type_check_expr ty iffalse ctx with
                    | [] -> Ok ty
                    | xs -> Err.wraps "False arm" xs
                | Error xs -> Err.wraps "True arm" xs
            | Ok _ -> Error (Err.one "Condition should be a boolean expression")
            | Error xs -> Err.wraps "Condition" xs
        | Expr.Try (expr, iferror) -> Error (Err.one "nyi")

    and type_check_expr (t: Type) (ex: Expr) (ctx: Context) : Res =
        match t, ex with
        | Type.String, Expr.String _ -> []
        | Type.Number, Expr.Number _ -> []
        | Type.Bool, Expr.Bool _ -> []
        | Type.Null, Expr.Null -> []
        | Type.Object ms, Expr.Object xs ->
            Seq.choose (
                Err.pickf
                    ( fun (KeyValue(prop, propType)) -> sprintf "Property '%s'" prop )
                    ( fun (KeyValue(prop, propType)) ->
                        if xs.ContainsKey(prop) then type_check_expr propType xs.[prop] ctx
                        else Err.one (sprintf "Missing property '%s': Expected a %O" prop propType)
                    )
            ) ms
            |> List.ofSeq
        | Type.Array ty, Expr.Array xs ->
            List.choose (
                Err.pickf
                    ( fun (i, x) -> sprintf "Item %i" i )
                    ( fun (i, x) -> type_check_expr ty x ctx )
            ) (List.indexed xs)
        | Type.Closure _, _ -> Err.one "nyi"
        | _, Expr.Piped_Input ->
            match Map.tryFind "" ctx.Variables with
            | Some (ty, _) -> List.choose id [Err.pick "Pipeline variable" (type_unify t ty)]
            | None -> Err.one "The pipeline variable does not exist in this context"
        | _, Expr.Variable v ->
            match Map.tryFind v ctx.Variables with
            | Some (ty, _) -> List.choose id [Err.pick (sprintf "Variable '%s'" v) (type_unify t ty)]
            | None -> Err.one (sprintf "Unrecognised variable: '%s'" v)
        | ty, Expr.Subscript (ex, sub) ->
            List.choose id
                [
                    Err.pick "Subscriptand" (type_check_expr (Type.Array ty) ex ctx)
                    Err.pick "Subscript index" (type_check_expr Type.Number sub ctx)
                ]
        | ty, Expr.Property (ex, prop) ->
            List.choose id
                [
                    Err.pick "Object expression" (type_check_expr (Type.Object (Map.ofList [(prop, ty)])) ex ctx)
                ]
        | ty, Expr.Evaluate_Command reqex -> 
            List.choose id
                [
                    Err.pick "Error in command call" (type_check_reqex ty reqex ctx)
                ]
        | ty, Expr.Cond (cond, iftrue, iffalse) ->
            List.choose id
                [
                    Err.pick "Condition" (type_check_expr Type.Bool cond ctx)
                    Err.pick "True arm" (type_check_expr ty iftrue ctx)
                    Err.pick "False arm" (type_check_expr ty iffalse ctx)
                ]
        | ty, Expr.Try (expr, iferror) ->
            List.choose id
                [
                    Err.pick "Main arm" (type_check_expr ty expr ctx)
                    Err.pick "Error arm" (type_check_expr ty iferror ctx)
                ]
        | Type.Any, _ -> []
        | _, _ -> Err.one (sprintf "Expected a %O but got: %O" t ex)

    and type_check_reqex (t: Type) (rx: CommandRequest) (ctx: Context) : Res =
        match Map.tryFind rx.Name ctx.Commands with
        | None -> Err.one (sprintf "Unrecognised command '%s'" rx.Name)
        | Some cmd ->

        let mutable args = rx.Args
        let mutable errors = []

        for (name, ty) in cmd.Signature.Args do
            match args with
            | x :: xs ->
                match type_check_expr ty x ctx with
                | [] -> ()
                | errs -> errors <- Err.wrap (sprintf "Argument '%s'" name) errs :: errors
                args <- xs
            | [] -> errors <- TypeError.Leaf (sprintf "Missing argument '%s': Expected a %O" name ty) :: errors

        for (name, ty) in cmd.Signature.OptArgs do
            match args with
            | x :: xs ->
                match type_check_expr ty x ctx with
                | [] -> ()
                | errs -> errors <- Err.wrap (sprintf "Optional argument '%s'" name) errs :: errors
                args <- xs
            | [] -> ()

        while args <> [] do
            errors <- TypeError.Leaf (sprintf "Unexpected argument %O" (List.head args)) :: errors
            args <- List.tail args

        for KeyValue (name, v) in rx.Flags do
            if cmd.Signature.Flags.ContainsKey name then
                match type_check_expr cmd.Signature.Flags.[name] v ctx with
                | [] -> ()
                | errs -> errors <- Err.wrap (sprintf "Flag '%s'" name) errs :: errors
            else errors <- TypeError.Leaf (sprintf "Unrecognised flag '%s'" name) :: errors

        match type_unify t cmd.Signature.ReturnType with
        | [] -> ()
        | errs -> errors <- Err.wrap (sprintf "Command return type error") errs :: errors

        errors

    and type_check_stmt (stmt: Statement) (ctx: Context) : Res =
        match stmt with
        | Statement.Declare (name, ty, expr) ->
            type_check_expr (Option.defaultValue Type.Any ty) expr ctx
        | Statement.Command req ->
            type_check_reqex Type.Any req ctx
        | Statement.Help _ -> []