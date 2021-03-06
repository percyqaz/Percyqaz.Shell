namespace Percyqaz.Shell.deprecated

open System

module Library =

    open Tree
    open FParsec

    type ShellResult<'T> =
        | Ok of 'T
        | ParseFail of string
        | TypeFail of Exception
        | RunFail of Exception

    module Help =

        let show_signature (ctx: Context) (name: string) (signature: CommandSignature) =
            ctx.WriteLine(sprintf "Showing help for '%s':\n" name)

            ctx.Write("usage: " + name)
            for arg, ty in signature.Args do
                ctx.Write(sprintf " <%s: %O>" arg ty)
            for arg, ty in signature.OptArgs do
                ctx.Write(sprintf " [%s: %O]" arg ty)
            ctx.WriteLine(" -> " + signature.ReturnType.ToString())

            if not signature.Flags.IsEmpty then
                ctx.WriteLine("  options:")
            for (KeyValue (flag, ty)) in signature.Flags do
                ctx.WriteLine(sprintf "    -%s : %O" flag ty)

            //todo: help text

    module Context =

        let rec do_stmt (stmt: StatementC) (ctx: Context) : Context =
            match stmt with
            | StatementC.Declare (name, ty, expr) ->
                { ctx with Variables = Map.add name (ty, eval_expr expr ctx) ctx.Variables }
            | StatementC.Eval expr -> 
                match eval_expr expr ctx with
                | Val.Null -> ()
                | x -> sprintf "%O" x |> ctx.WriteLine
                ctx
            | StatementC.Help (Some command) ->
                match Map.tryFind command ctx.Commands with
                | Some c -> Help.show_signature ctx command c.Signature
                | None -> ctx.WriteLine(sprintf "Unrecognised command: '%s'" command)
                ctx
            | StatementC.Help None ->
                ctx.WriteLine(sprintf "Available commands: %s" (String.concat ", " (ctx.Commands.Keys)))
                ctx

        and eval_expr (ex: ExprC) (ctx: Context) : Val =
            match ex with
            
            | ExprC.String s -> Val.String s
            | ExprC.Number n -> Val.Number n
            | ExprC.Bool b -> Val.Bool b
            | ExprC.Null -> Val.Null
            | ExprC.Object m -> m |> Map.map (fun _ ex -> eval_expr ex ctx) |> Val.Object
            | ExprC.Array xs ->  xs |> List.map (fun ex -> eval_expr ex ctx) |> Val.Array

            | ExprC.Pipeline (head, rest) -> 
                let hvalue = eval_expr head ctx
                eval_expr rest (ctx.WithPipelineValue hvalue)
            | ExprC.Pipeline_Variable -> 
                match Map.tryFind "" ctx.Variables with
                | Some (ty, v) -> v
                | None -> failwith "The pipeline variable does not exist in this context"
            | ExprC.Variable x -> 
                match Map.tryFind x ctx.Variables with
                | Some (ty, v) -> v
                | None -> failwithf "Unrecognised variable: '%s'" x
            | ExprC.Subscript (main, sub) -> 
                let m = eval_expr main ctx
                let s = eval_expr sub ctx
                match m with
                | Val.Array xs ->
                    match s with
                    | Val.Number f -> xs.[int f]
                    | _ -> failwith "Must subscript arrays with a number"
                | _ -> failwith "This value is not subscriptable"
            | ExprC.Property (main, prop) ->
                let m = eval_expr main ctx
                match m with
                | Val.Object ms ->
                    if ms.ContainsKey prop then ms.[prop]
                    else failwithf "Object has no such property '%s'" prop
                | _ -> failwith "This value is not an object"
            | ExprC.Command (rx: CommandRequestC) ->
                dispatch rx ctx // todo: wrap exception that is thrown
            | ExprC.Cond (arms, basecase) ->
                let rec loop arms =
                    match arms with
                    | (cond, ex) :: arms ->
                        let c = eval_expr cond ctx
                        if (match c with Val.Bool b -> b | _ -> failwith "Condition must be a boolean") then
                            eval_expr ex ctx
                        else loop arms
                    | [] -> eval_expr basecase ctx
                loop arms
            | ExprC.Try (ex, iferror) -> failwith "nyi"
            | ExprC.Block (stmts, expr) ->
                let rec loop stmts ctx =
                    match stmts with
                    | [] -> ctx
                    | stmt :: xs -> loop xs (do_stmt stmt ctx)
                eval_expr expr (loop stmts ctx)
            | ExprC.Lambda (binds, returnType, body) ->
                Val.Lambda ({ Args = binds; OptArgs = List.empty; Flags = Map.empty; ReturnType = returnType }, body, ctx)
            | ExprC.Call_Lambda (l, args, flags) ->
                match eval_expr l ctx with
                | Val.Lambda (sgn, body, captured_ctx) ->
                    let mutable args = args
                    let mutable lctx = captured_ctx
                    for (argName, ty) in sgn.Args do
                        lctx <- lctx.WithVar(argName, eval_expr (List.head args) ctx, ty)
                        args <- List.tail args
                    for (argName, ty) in sgn.OptArgs do
                        if not args.IsEmpty then
                            lctx <- lctx.WithVar(argName, eval_expr (List.head args) ctx, ty)
                            args <- List.tail args
                    // todo: flags (we don't care rn)
                    eval_expr body lctx
                | _ -> failwith "impossible after type checks"

        and dispatch (req: CommandRequestC) (ctx: Context) : Val =
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
            try 
                let chk = Check.type_check_command Type.Any command this
                try Context.dispatch chk this |> Ok
                with err -> RunFail err
            with exn -> TypeFail exn
                
        member this.Execute(command: string) : ShellResult<Val> =
            match run (Parser.parse_command .>> eof) command with
            | Success (res, _, _) -> this.Execute res
            | Failure (err, _, _) -> ParseFail err

        member this.Evaluate(ex: Expr) : ShellResult<Val> =
            try
                let chk = Check.type_check_expr Type.Any ex this
                try Context.eval_expr chk this |> Ok
                with err -> RunFail err
            with exn -> TypeFail exn
            
        member this.Evaluate(command: string) : ShellResult<Val> =
            match run (Parser.parse_expr_ext .>> eof) command with
            | Success (res, _, _) -> this.Evaluate res
            | Failure (err, _, _) -> ParseFail err

        member this.Interpret(stmt: Statement) : ShellResult<Context> =
            try
                let chk, _ = Check.type_check_stmt stmt this
                try Context.do_stmt chk this |> Ok
                with err -> RunFail err
            with exn -> TypeFail exn

        member this.Interpret(command: string) : ShellResult<Context> =
            match run (Parser.parse_toplevel_stmt .>> eof) command with
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
                | TypeFail exn -> ctx.WriteLine (sprintf "Type error: %s" exn.Message)
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
                | TypeFail exn -> printfn "Type failure: %s" exn.Message
                | RunFail exn -> printfn "Runtime failure: %s" exn.Message