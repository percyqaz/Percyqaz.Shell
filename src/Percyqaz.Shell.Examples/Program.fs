open Percyqaz.Shell.Library
open Percyqaz.Shell.Tree
open System.Runtime.InteropServices

type Test =

    static member Add_Default_Five (x: int, [<Optional; DefaultParameterValue(5)>] y: int) = x + y
        
    static member Echo (input: Val) : unit = printfn "%O" input

    static member Sum (xs: int array) = Array.sum xs

    static member Host () =
        printfn "serving"
        ShellInterface.serve "SHELL-TEST" (Context.Create<Test>())
        printfn "not serving"

    static member Connect (command: string) =
        ShellInterface.connect "SHELL-TEST" command

ShellInterface.basic_repl<Test>()