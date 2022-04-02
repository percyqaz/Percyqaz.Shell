namespace Percyqaz.Shell.v2

open System
open FParsec
open Tree
open Runtime
open Parser

type ShellResult<'T> =
    | Ok of 'T
    | ParseFail of ParserError
    | RunFail of Exception

module Library =

    type Context with

        member this.Evaluate(ex: Expr) : ShellResult<Val> =
            try eval_expr ex this |> Ok
            with err -> RunFail err
    
        member this.Evaluate(query: string) : ShellResult<Val> =
            match run (parse_expr_ext .>> eof) query with
            | Success (res, _, _) -> this.Evaluate res
            | Failure (_, err, _) -> ParseFail err

        member this.Interpret(stmt: Stmt) : ShellResult<Context> =
            try do_stmt stmt this |> Ok
            with err -> RunFail err

        member this.Interpret(query: string) : ShellResult<Context> =
            match run (parse_toplevel_stmt .>> eof) query with
            | Success (res, _, _) -> this.Interpret res
            | Failure (_, err, _) -> ParseFail err

    module Interface =
        open System.IO
        open System.IO.Pipes

        let serve (id: string) (ctx: Context) =
            use server = new NamedPipeServerStream(id)

            while true do
                server.WaitForConnection()

                let reader = new StreamReader(server)
                let writer = new StreamWriter(server)

                let ctx = { ctx with IO = { In = reader; Out = writer } }

                match ctx.Interpret(ctx.ReadLine()) with
                | Ok c -> ()
                | ParseFail err -> ctx.WriteLine (sprintf "%O" err.Messages.Head)
                | RunFail exn -> ctx.WriteLine (sprintf "%s" exn.Message)
            
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

        let basic_repl<'T>() =
            let mutable ctx = Context.Empty //
            while true do 
                printf "> "
                match ctx.Interpret(ctx.ReadLine()) with
                | Ok c -> ctx <- c
                | ParseFail err -> printfn "%O" err
                | RunFail exn -> printfn "%s" exn.Message