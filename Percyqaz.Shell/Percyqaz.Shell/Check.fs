namespace Percyqaz.Shell

module Check =

    open Tree

    // todo: type checking should perform casting too
    let rec type_check (t: Type) (v: Val) : Result<unit, string list> =
        match t, v with
        | Type.Any, _ -> Ok()
        | Type.String, Val.String _ -> Ok()
        | Type.Bool, Val.Bool _ -> Ok()
        | Type.Number, Val.Number _ -> Ok()
        | Type.Object ms, Val.Object vs ->
            let errors =
                Seq.fold 
                    ( fun errs (KeyValue (prop, propType)) ->
                        if vs.ContainsKey(prop) then
                            match type_check propType vs.[prop] with
                            | Ok _ -> errs
                            | Error es -> (List.map (fun s -> sprintf "Property '%s': %s" prop s) es) @ errs
                        else sprintf "Missing property '%s': Expected a %O" prop propType :: errs
                    )
                    [] ms
            if errors.IsEmpty then Ok() else Error errors
        | Type.Array ty, Val.Array xs ->
            let errors =
                List.fold
                    ( fun errs (i, x) ->
                        match type_check ty x with
                        | Ok _ -> errs
                        | Error es -> (List.map (fun s -> sprintf "Element %i: %s" i s) es) @ errs
                    )
                    [] (List.indexed xs)
            if errors.IsEmpty then Ok() else Error errors
        | Type.Unit, Val.Unit -> Ok()
        | Type.Closure, Val.Closure _ -> Ok()
        | _ -> Error [sprintf "Expected a %O, got %O" t v]

    let rec type_check_req (cmd: Command) (req: CommandRequest) : Result<unit, string list> =
        let mutable args = req.Args
        let mutable errors = []

        for (name, ty) in cmd.Signature.Args do
            match args with
            | x :: xs ->
                match type_check ty x with
                | Ok() -> ()
                | Error errs -> errors <- (List.map (fun s -> sprintf "Argument '%s': %s" name s) errs) @ errors
                args <- xs
            | [] -> errors <- sprintf "Missing argument '%s': Expected a %O" name ty :: errors

        for (name, ty, _) in cmd.Signature.OptArgs do
            match args with
            | x :: xs ->
                match type_check ty x with
                | Ok() -> ()
                | Error errs -> errors <- (List.map (fun s -> sprintf "Optional argument '%s': %s" name s) errs) @ errors
                args <- xs
            | [] -> ()

        while args <> [] do
            errors <- sprintf "Unexpected argument %O" (List.head args) :: errors
            args <- List.tail args

        for KeyValue (name, v) in req.Flags do
            if cmd.Signature.Flags.ContainsKey name then
                match type_check (fst cmd.Signature.Flags.[name]) v with
                | Ok() -> ()
                | Error errs -> errors <- (List.map (fun s -> sprintf "Flag '%s': %s" name s) errs) @ errors
            else errors <- sprintf "Unrecognised flag '%s'" name :: errors
        if errors.IsEmpty then Ok() else Error errors