open Percyqaz.Shell.Library
open Percyqaz.Shell.Tree
open System.Runtime.InteropServices

let mutable int = 0

type Test =

    static member Add_Default_Five (x: int, [<Optional; DefaultParameterValue(5)>] y: int) = x + y
        
    static member Echo (input: Val) : unit = printfn "%O" input

    static member Increment () : int = int <- int + 1; int

mainloop<Test>()