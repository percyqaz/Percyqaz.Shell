namespace Percyqaz.Shell

open System
open FParsec
open Percyqaz.Shell.Tree
open Percyqaz.Shell.Runtime
open Percyqaz.Shell.Parser

type ShellResult<'T> =
    | Ok of 'T
    | ParseFail of ParserError
    | RunFail of Exception

[<AutoOpen>]
module Extensions =

    type Context with

        member this.Evaluate(ex: Expr) : ShellResult<Val> =
            try
                eval_expr ex this |> Ok
            with err ->
                RunFail err

        member this.Evaluate(query: string) : ShellResult<Val> =
            match run (parse_expr_ext .>> eof) query with
            | Success(res, _, _) -> this.Evaluate res
            | Failure(_, err, _) -> ParseFail err

        member this.Interpret(stmt: Stmt) : ShellResult<Context> =
            try
                do_stmt stmt this true |> Ok
            with err ->
                RunFail err

        member this.Interpret(query: string) : ShellResult<Context> =
            match run (parse_toplevel_stmt .>> eof) query with
            | Success(res, _, _) -> this.Interpret res
            | Failure(_, err, _) -> ParseFail err

        member this.RunScript(script: string) : ShellResult<Context> =
            match run parse_script script with
            | Success(res, _, _) ->
                List.fold
                    (fun state stmt ->
                        match state with
                        | Ok ctx -> ctx.Interpret stmt
                        | _ -> state
                    )
                    (Ok this)
                    res
            | Failure(_, err, _) -> ParseFail err

module Shell =

    open System.IO
    open System.IO.Pipes

    let serve (id: string) (ctx: Context) =
        use server = new NamedPipeServerStream(id)

        while true do
            server.WaitForConnection()

            let reader = new StreamReader(server)
            let writer = new StreamWriter(server)

            let ctx =
                { ctx with
                    IO = { In = reader; Out = writer }
                }

            match ctx.Interpret(ctx.ReadLine()) with
            | Ok c -> ()
            | ParseFail err -> ctx.WriteLine(sprintf "%O" err.Messages.Head)
            | RunFail exn -> ctx.WriteLine(sprintf "%s" exn.Message)

            writer.Flush()

            server.Disconnect()

    let connect (id: string) (query: string) =
        use client = new NamedPipeClientStream(id)

        client.Connect(1000)

        let reader = new StreamReader(client)
        let writer = new StreamWriter(client)

        writer.WriteLine query
        writer.Flush()

        printfn "%s" (reader.ReadToEnd())

    let basic_repl (ctx: Context) =
        let mutable ctx = ctx

        while true do
            printf "> "

            match ctx.Interpret(ctx.ReadLine().Trim()) with
            | Ok c -> ctx <- c
            | ParseFail err -> printfn "%O" err
            | RunFail exn -> printfn "%s" exn.Message

type Context = Tree.Context
