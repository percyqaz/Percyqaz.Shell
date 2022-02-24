namespace Percyqaz.Shell

open Tree
open System.Runtime.InteropServices

module Helpers =

    let echo : CommandInfo =
        {
            Signature =
                {
                    Args = []
                    OptArgs = []
                    Flags = Map.empty
                    ReturnType = Type.Null
                }
            Implementation = Unchecked.defaultof<_>
        }

    type Test =
        static member AddFive(x: int, [<Optional; DefaultParameterValue(5)>] y: int) : int = x + 5
        member this.AddTwo(x: int) : int = x + 2

    let create<'T>() =
        let methods = typeof<'T>.GetMethods()
        for m in methods do
            if m.IsStatic then
                printfn "name: %s" m.Name
                for p in m.GetParameters() do
                    printfn "argument %s: type %O" p.Name p.ParameterType
                printfn "return type: %O" m.ReturnType

