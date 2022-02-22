namespace Percyqaz.Shell

module Check =

    open Tree

    [<RequireQualifiedAccess>]
    type ChkErr =
        | Leaf of msg: string
        | Node of name: string * ChkErr list // list should be nonempty
    type Res = ChkErr list

    module Err =
        let wrap name e = ChkErr.Node (name, e)

        let pick name errs =
            match errs with
            | [] -> None
            | xs -> Some (wrap name xs)

        let pickf namefunc errfunc o =
            match errfunc o with
            | [] -> None
            | xs -> Some (wrap (namefunc o) xs)

        let one s = [ChkErr.Leaf s]

        let prettyPrint e =
            let rec loop depth e =
                match e with
                | ChkErr.Leaf m -> printfn "%s%s" (String.replicate depth " ") m
                | ChkErr.Node (name, children) ->
                    printfn "%s@%s:" (String.replicate depth " ") name
                    for c in children do loop (depth + 1) c
            loop 0 e

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
        | _, _ -> Err.one (sprintf "Cannot unify type %O with expected %O" current target)

    let rec type_check_expr (t: Type) (ex: Expr) (ctx: Context) : Res =
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
        | _, Expr.Piped_Input -> Err.one "nyi"
        | _, Expr.Variable v -> Err.one "nyi"
        | ty, Expr.Subscript (ex, sub) ->
            List.choose id
                [
                    Err.pick "Main expression" (type_check_expr (Type.Array ty) ex ctx)
                    Err.pick "Subscript expression" (type_check_expr Type.Number sub ctx)
                ]
        | ty, Expr.Property (ex, prop) ->
            List.choose id
                [
                    Err.pick "Property of object" (type_check_expr (Type.Object (Map.ofList [(prop, ty)])) ex ctx)
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

    and type_check_reqex (t: Type) (rx: CommandRequestEx) (ctx: Context) : Res =
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
            | [] -> errors <- ChkErr.Leaf (sprintf "Missing argument '%s': Expected a %O" name ty) :: errors

        for (name, ty, _) in cmd.Signature.OptArgs do
            match args with
            | x :: xs ->
                match type_check_expr ty x ctx with
                | [] -> ()
                | errs -> errors <- Err.wrap (sprintf "Optional argument '%s'" name) errs :: errors
                args <- xs
            | [] -> ()

        while args <> [] do
            errors <- ChkErr.Leaf (sprintf "Unexpected argument %O" (List.head args)) :: errors
            args <- List.tail args

        for KeyValue (name, v) in rx.Flags do
            if cmd.Signature.Flags.ContainsKey name then
                match type_check_expr (fst cmd.Signature.Flags.[name]) v ctx with
                | [] -> ()
                | errs -> errors <- Err.wrap (sprintf "Flag '%s'" name) errs :: errors
            else errors <- ChkErr.Leaf (sprintf "Unrecognised flag '%s'" name) :: errors

        match type_unify t cmd.Signature.ReturnType with
        | [] -> ()
        | errs -> errors <- Err.wrap (sprintf "Command return type error") errs :: errors

        errors