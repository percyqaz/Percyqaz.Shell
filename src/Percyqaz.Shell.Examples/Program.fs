open Percyqaz.Shell

let context =
    Context.Empty.WithCommand("sum",
        Command.create
            "Sums all the numbers in a list"
            ["xs"]
            (Impl.Create(
                Types.list Types.num,
                (fun (xs: float list) -> List.sum xs),
                Types.num
            ))
    )

Shell.basic_repl(context)