namespace Percyqaz.Shell

open System

module Library =

    open Tree
    open FParsec

    type ShellResult<'T> =
        | Ok of 'T
        | ParseFail of string
        | TypeFail of Check.TypeError list
        | RunFail of Exception

    module Help =

        let show_signature (ctx: Context) (name: string) (signature: CommandSignature) =
            ctx.WriteLine(sprintf "Showing help for '%s':\n" name)

            ctx.Write("usage: " + name)
            for arg, ty in signature.Args do
                ctx.Write(" " + arg)
            for arg, ty in signature.OptArgs do
                ctx.Write(" [" + arg + "]")
            ctx.WriteLine("")

            if not signature.Flags.IsEmpty then
                ctx.WriteLine("  options:")
            for (KeyValue (flag, ty)) in signature.Flags do
                ctx.WriteLine("    -" + flag)

            //todo: type information and help text

    module Context =

        let rec do_stmt (stmt: Statement) (ctx: Context) : Context =
            match stmt with
            | Statement.Declare (name, ty, expr) ->
                let vtype =
                    match ty with
                    | Some t -> t
                    | None -> 
                        match Check.infer_type expr ctx with
                        | Result.Ok t -> t
                        | Result.Error xs -> failwithf "Could not infer type for variable %s: %A" name xs
                { ctx with Variables = Map.add name (vtype, eval_expr expr ctx) ctx.Variables }
            | Statement.Command rx -> 
                // todo: only auto-echo if turned on
                match dispatch rx ctx with
                | Val.Null -> ()
                | x -> sprintf "%O" x |> ctx.WriteLine
                ctx
            | Statement.Help (Some command) ->
                match Map.tryFind command ctx.Commands with
                | Some c -> Help.show_signature ctx command c.Signature
                | None -> ctx.WriteLine(sprintf "Unrecognised command: '%s'" command)
                ctx
            | Statement.Help None ->
                ctx.WriteLine(sprintf "Available commands: %s" (String.concat ", " (ctx.Commands.Keys)))
                ctx

        and eval_expr (ex: Expr) (ctx: Context) : Val =
            match ex with
            
            | Expr.String s -> Val.String s
            | Expr.Number n -> Val.Number n
            | Expr.Bool b -> Val.Bool b
            | Expr.Null -> Val.Null
            | Expr.Object m -> m |> Map.map (fun _ ex -> eval_expr ex ctx) |> Val.Object
            | Expr.Array xs ->  xs |> List.map (fun ex -> eval_expr ex ctx) |> Val.Array
            | Expr.Closure x ->
                match eval_expr x ctx with
                | Val.Closure req -> Val.Closure req
                | x -> failwithf "Expected a closure here but got %A" x

            | Expr.Piped_Input -> 
                match Map.tryFind "" ctx.Variables with
                | Some (ty, v) -> v
                | None -> failwith "The pipeline variable does not exist in this context"
            | Expr.Variable x -> 
                match Map.tryFind x ctx.Variables with
                | Some (ty, v) -> v
                | None -> failwithf "Unrecognised variable: '%s'" x
            | Expr.Subscript (main, sub) -> 
                let m = eval_expr main ctx
                let s = eval_expr sub ctx
                match m with
                | Val.Array xs ->
                    match s with
                    | Val.Number f -> xs.[int f]
                    | _ -> failwith "Must subscript arrays with a number"
                | _ -> failwith "This value is not subscriptable"
            | Expr.Property (main, prop) ->
                let m = eval_expr main ctx
                match m with
                | Val.Object ms ->
                    if ms.ContainsKey prop then ms.[prop]
                    else failwithf "Object has no such property '%s'" prop
                | _ -> failwith "This value is not an object"
            | Expr.Evaluate_Command (rx: CommandRequest) ->
                dispatch rx ctx // todo: wrap exception that is thrown
            | Expr.Cond (cond, iftrue, iffalse) ->
                let c = eval_expr cond ctx
                if (match c with Val.Bool b -> b | _ -> failwith "Condition must be a boolean") then
                    eval_expr iftrue ctx
                else eval_expr iffalse ctx
            | Expr.Try (ex, iferror) -> failwith "nyi"

        and dispatch (req: CommandRequest) (ctx: Context) : Val =
            let cmd = ctx.Commands.[req.Name]
            cmd.Implementation
                {
                    Args = req.Args |> List.map (fun ex -> eval_expr ex ctx)
                    Flags = req.Flags |> Map.map (fun _ ex -> eval_expr ex ctx)
                    IOContext = ctx.IO
                }

    type Context with
        static member Create<'T>() =
            let mutable ctx = Context.Empty
            for (name, cmd) in DotNetInterop.createCommands<'T>() do
                ctx <- ctx.AddCommand(name, cmd)
            ctx

        member this.Execute(command: CommandRequest) : ShellResult<Val> =
            match Check.type_check_reqex Type.Any command this with
            | [] -> 
                try Context.dispatch command this |> Ok
                with err -> RunFail err
            | xs -> TypeFail xs
                
        member this.Execute(command: string) : ShellResult<Val> =
            match run (Parser.commandParser .>> eof) command with
            | Success (res, _, _) -> this.Execute res
            | Failure (err, _, _) -> ParseFail err

        member this.Evaluate(ex: Expr) : ShellResult<Val> =
            match Check.type_check_expr Type.Any ex this with
            | [] -> 
                try Context.eval_expr ex this |> Ok
                with err -> RunFail err
            | xs -> TypeFail xs
            
        member this.Evaluate(command: string) : ShellResult<Val> =
            match run (Parser.exprParser .>> eof) command with
            | Success (res, _, _) -> this.Evaluate res
            | Failure (err, _, _) -> ParseFail err

        member this.Interpret(stmt: Statement) : ShellResult<Context> =
            match Check.type_check_stmt stmt this with
            | [] -> 
                try Context.do_stmt stmt this |> Ok
                with err -> RunFail err
            | xs -> TypeFail xs

        member this.Interpret(command: string) : ShellResult<Context> =
            match run (Parser.stmtParser .>> eof) command with
            | Success (res, _, _) -> this.Interpret res
            | Failure (err, _, _) -> ParseFail err

        member this.AddCommand(name: string, details: CommandInfo) =
            { this with Commands = Map.add name details this.Commands }

    module ShellInterface =
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
                | ParseFail err -> ctx.WriteLine err
                | TypeFail errs -> for s in Check.Err.format errs do ctx.WriteLine s
                | RunFail exn -> ctx.WriteLine (sprintf "Error: %s" exn.Message)
                
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
            let mutable ctx = Context.Create<'T>()
            while true do 
                printf "> "
                match ctx.Interpret(ctx.ReadLine()) with
                | Ok c -> ctx <- c
                | ParseFail err -> printfn "%s" err
                | TypeFail errs -> for s in Check.Err.format errs do printfn "%s" s
                | RunFail exn -> printfn "Runtime failure: %s" exn.Message