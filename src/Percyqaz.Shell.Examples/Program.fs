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
    ).WithCommand("map",
        Command.create
            "Applies a mapping to an array, returning a new array"
            ["f"; "xs"]
            (Impl.Create(
                FuncTypes.func1(Types.any, Types.any),
                Types.list Types.any,
                (fun (f: Tree.Val -> Tree.Val) (xs: Tree.Val list) -> List.map f xs),
                Types.list Types.any
            ))
    ).WithCommand("iter",
        Command.create
            "Applies a function to each item in the array"
            ["f"; "xs"]
            (Impl.Create(
                FuncTypes.func1(Types.any, Types.nil),
                Types.list Types.any,
                (fun (f: Tree.Val -> unit) (xs: Tree.Val list) -> List.iter f xs)
            ))
    )

Shell.basic_repl(context)