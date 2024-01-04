open Percyqaz.Shell

let ctx =
    ShellContext.Empty
        .WithCommand("echo", "Echoes its input back to the console", "message", printfn "%s")
        .WithCommand("add", "Adds two numbers together", "a", "b", (fun (a: float) (b: float) -> a + b))
        .WithCommand(
            "list_add",
            "Adds a number to all items in a list",
            "a",
            "xs",
            fun (a: float) (xs: float list) -> List.map (fun x -> x + a) xs
        )


let token = Shell.IPC.start_server_thread "Percyqaz.Shell" ctx

while true do
    Shell.IPC.send "Percyqaz.Shell" (System.Console.ReadLine()) |> printfn "%s"

//Shell.repl ctx
