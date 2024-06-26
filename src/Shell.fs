﻿namespace Percyqaz.Shell

open System
open Percyqaz.Shell.Data
open FParsec

type ShellContext =
    {
        Commands: Map<string, Func>
    }
    static member Empty = { Commands = Map.empty }

    member this.WithCommand(name: string, func: Func) =
        {
            Commands = Map.add name func this.Commands
        }

    member this.WithCommand(name: string, desc: string, impl: unit -> 'T) =
        this.WithCommand(name, Command.create desc [ "" ] (Impl.Create1 impl))

    member this.WithIOCommand(name: string, desc: string, impl: IOContext -> 'T) =
        this.WithCommand(name, Command.create desc [ "" ] (Impl.Create1Io(fun io () -> impl io)))

    member this.WithCommand(name: string, desc: string, arg1name, impl: 'A -> 'T) =
        this.WithCommand(name, Command.create desc [ arg1name ] (Impl.Create1 impl))

    member this.WithIOCommand(name: string, desc: string, arg1name, impl: IOContext -> 'A -> 'T) =
        this.WithCommand(name, Command.create desc [ arg1name ] (Impl.Create1Io impl))

    member this.WithCommand(name: string, desc: string, arg1name, arg2name, impl: 'A -> 'B -> 'T) =
        this.WithCommand(name, Command.create desc [ arg1name; arg2name ] (Impl.Create2 impl))

    member this.WithIOCommand(name: string, desc: string, arg1name, arg2name, impl: IOContext -> 'A -> 'B -> 'T) =
        this.WithCommand(name, Command.create desc [ arg1name; arg2name ] (Impl.Create2Io impl))

    member this.WithCommand(name: string, desc: string, arg1name, arg2name, arg3name, impl: 'A -> 'B -> 'C -> 'T) =
        this.WithCommand(name, Command.create desc [ arg1name; arg2name; arg3name ] (Impl.Create3 impl))

    member this.WithIOCommand
        (
            name: string,
            desc: string,
            arg1name,
            arg2name,
            arg3name,
            impl: IOContext -> 'A -> 'B -> 'C -> 'T
        ) =
        this.WithCommand(name, Command.create desc [ arg1name; arg2name; arg3name ] (Impl.Create3Io impl))

    member this.WithCommand
        (
            name: string,
            desc: string,
            arg1name,
            arg2name,
            arg3name,
            arg4name,
            impl: 'A -> 'B -> 'C -> 'D -> 'T
        ) =
        this.WithCommand(name, Command.create desc [ arg1name; arg2name; arg3name; arg4name ] (Impl.Create4 impl))

    member this.WithIOCommand
        (
            name: string,
            desc: string,
            arg1name,
            arg2name,
            arg3name,
            arg4name,
            arg5name,
            impl: 'A -> 'B -> 'C -> 'D -> 'E -> 'T
        ) =
        this.WithCommand(
            name,
            Command.create desc [ arg1name; arg2name; arg3name; arg4name; arg5name ] (Impl.Create5 impl)
        )


module Shell =

    [<RequireQualifiedAccess>]
    type ShellRequest =
        | Help of string option
        | Command of string * Val list

    module Parser =

        let private string_literal =

            let escape =
                anyOf "\"\\/bfnrt"
                |>> function
                    | 'b' -> "\b"
                    | 'f' -> "\u000C"
                    | 'n' -> "\n"
                    | 'r' -> "\r"
                    | 't' -> "\t"
                    | c -> string c

            let unicode_escape =
                let hex_to_int c = (int c &&& 15) + (int c >>> 6) * 9

                pstring "u"
                >>. pipe4
                    hex
                    hex
                    hex
                    hex
                    (fun h3 h2 h1 h0 ->
                        (hex_to_int h3) * 4096
                        + (hex_to_int h2) * 256
                        + (hex_to_int h1) * 16
                        + hex_to_int h0
                        |> char
                        |> string
                    )

            let escaped_char_snippet = pstring "\\" >>. (escape <|> unicode_escape)
            let normal_char_snippet = manySatisfy (fun c -> c <> '"' && c <> '\\')

            between (pchar '"') (pchar '"') (stringsSepBy normal_char_snippet escaped_char_snippet)

        let parse_text: Parser<Val, unit> =
            string_literal <|> many1Satisfy (isNoneOf " \t\n") |>> Val.Text <?> "Text value"

        let parse_number: Parser<Val, unit> =
            tuple3 (opt (pchar '-' >>% -1.0) |>> Option.defaultValue 1.0) (many1Satisfy isDigit) (opt (pchar '.' >>. many1Satisfy isDigit))
            |>> function
                | (sign, pre, Some post) -> System.Double.TryParse (pre + "." + post) |> snd |> fun x -> x * sign
                | (sign, pre, None) -> System.Double.TryParse (pre) |> snd |> fun x -> x * sign
            |>> Val.Num
            <?> "Number value"

        let parse_nil: Parser<Val, unit> = preturn Val.Nil

        let parse_any = parse_number <|> parse_text

        let rec parse_choices (cases: (string * Type) list) : Parser<Val, unit> =
            match cases with
            | (label, Type.Nil) :: xs ->
                pstringCI label <|> pstringCI ("'" + label) <?> label
                >>% Val.Atom(label, Val.Nil)
                <|> parse_choices xs
            | (label, ty) :: xs ->
                ((pstringCI label <|> pstringCI ("'" + label)) <?> label
                 >>. spaces
                 >>. pchar '('
                 >>. spaces
                 >>. parse_by_type ty
                 |>> fun v -> Val.Atom(label, v))
                <|> parse_by_type ty
                |>> fun v -> Val.Atom(label, v)
                <|> parse_choices xs
            | [] -> pzero

        and parse_object (os: Map<string, Type>) =
            let keyValue =
                os
                |> Map.toSeq
                |> Seq.map (fun (key, ty) ->
                    pstringCI key .>>. (spaces >>. pstring ":" >>. spaces >>. parse_by_type ty)
                )
                |> choice

            pchar '{' >>. spaces >>. sepBy (keyValue .>> spaces) (pchar ',' >>. spaces)
            .>> pchar '}'
            |>> (Map.ofSeq >> Val.Obj)
            <?> "Object value"

        and parse_by_type (ty: Type) =
            match ty with
            | Type.Any -> parse_any
            | Type.Number -> parse_number
            | Type.Text -> parse_text
            | Type.Nil -> parse_nil

            | Type.Atoms choices -> parse_choices choices
            | Type.Object os -> parse_object os
            | Type.Array array_ty ->
                pchar '['
                >>. spaces
                >>. sepBy (parse_by_type array_ty .>> spaces) (pchar ',' >>. spaces)
                .>> pchar ']'
                |>> Val.Arr
            | Type.Function _ -> failwithf "Type %O cannot be parsed from command line arguments" ty

        let parse_command (name: string, func: Func) =
            let rec args xs =
                match xs with
                | (_, ty) :: xs -> spaces >>. parse_by_type ty >>= fun v -> args xs |>> fun rest -> v :: rest
                | [] -> preturn []

            (pstringCI name |>> fun x -> x.ToLower()) .>>. args (fst func.Signature)
            .>> spaces
            .>> eof

        let build (ctx: ShellContext) =
            let rec cmds (commands: (string * Func) list) =
                match commands with
                | (name, func) :: xs -> parse_command (name, func) |>> ShellRequest.Command <|> cmds xs
                | [] -> pzero

            cmds (ctx.Commands |> Map.toList)
            <|> (pstringCI "help" >>. (spaces >>. opt (identifier (IdentifierOptions())))
                 |>> ShellRequest.Help)
            <?> "The name of a command or 'help'"

    let dispatch (cmd: ShellRequest) (ctx: ShellContext) (io: IOContext) =
        match cmd with
        | ShellRequest.Command(name, args) ->
            try
                let result = ctx.Commands.[name].Impl io args

                if result <> Val.Nil then
                    io.WriteLine(result.ToString())
            with err ->
                io.WriteLine err.Message
        | ShellRequest.Help None ->
            io.WriteLine(sprintf "Available commands: %s" (String.concat ", " (ctx.Commands |> Map.keys)))
        | ShellRequest.Help(Some cmd) ->
            match ctx.Commands.TryFind cmd with
            | None -> io.WriteLine(sprintf "No such command '%s'" cmd)
            | Some func ->
                io.WriteLine(sprintf "Showing help for '%s':\n" cmd)

                io.Write("usage: " + cmd)

                for arg, ty in fst func.Signature do
                    if ty <> Type.Nil then
                        io.Write(sprintf " <%s: %O>" arg ty)

                let ret = snd func.Signature

                if ret <> Type.Nil then
                    io.Write(sprintf " -> %O" ret)

                io.WriteLine("\n" + func.Desc)

    let repl (io: IOContext) (ctx: ShellContext) =
        let parser = Parser.build ctx

        while true do
            io.Write "> "

            match run parser (io.ReadLine().Trim()) with
            | Success(res, _, _) -> dispatch res ctx io
            | Failure(err, _, _) ->
                let error_message = err.ToString()

                let error_message =
                    error_message
                        .Substring(error_message.IndexOf('\n') + 1)
                        .Replace("Note: The error occurred at the end of the input stream.\r\n", "")

                let error_message =
                    error_message.Substring(error_message.IndexOf('\n') + 1).TrimEnd()

                io.WriteLine(sprintf "  %s" error_message)

    type ShellContext with

        member this.Evaluate (io: IOContext) (input: string) =
            let parser = Parser.build this

            match run parser (input.Trim()) with
            | Success(res, _, _) -> dispatch res this io
            | Failure(err, _, _) ->
                let error_message = err.ToString()

                let error_message =
                    error_message
                        .Substring(error_message.IndexOf('\n') + 1)
                        .Replace("Note: The error occurred at the end of the input stream.\r\n", "   ")

                let error_message =
                    error_message.Substring(error_message.IndexOf('\n') + 1).TrimEnd()

                io.WriteLine(sprintf "  %s" error_message)

    open System.IO
    open System.IO.Pipes
    open System.Threading

    module IPC =

        /// This modifies the context to use a pipe as IO
        let start_server (name: string) (ctx: ShellContext) (cancellationToken: CancellationToken) =
            task {
                let server = new NamedPipeServerStream(name, PipeDirection.InOut, 1)

                let reader = new StreamReader(server, Text.Encoding.UTF8)

                let io =
                    {
                        In = reader
                        Out = new StreamWriter(server)
                    }

                let parser = Parser.build ctx
                let BOM = char 65279

                try

                    while not cancellationToken.IsCancellationRequested do
                        do! server.WaitForConnectionAsync(cancellationToken)
                        let! request = io.In.ReadLineAsync()

                        match run parser (request.Trim().Trim(BOM)) with
                        | Success(res, _, _) -> dispatch res ctx io
                        | Failure(err, _, _) ->
                            let error_message = err.ToString()

                            let error_message =
                                error_message
                                    .Substring(error_message.IndexOf('\n') + 1)
                                    .Replace("Note: The error occurred at the end of the input stream.\r\n", "")

                            let error_message =
                                error_message.Substring(error_message.IndexOf('\n') + 1).TrimEnd()

                            io.WriteLine(error_message)

                        io.Out.Flush()

                        server.WaitForPipeDrain()
                        server.Disconnect()

                with
                | :? OperationCanceledException
                | :? Tasks.TaskCanceledException -> ()

            }
            |> Async.AwaitTask
            |> Async.RunSynchronously

        /// This modifies the context to use a pipe as IO
        let start_server_thread (name: string) (ctx: ShellContext) =
            let cts = new CancellationTokenSource()
            let thread = new Thread(fun () -> start_server name ctx cts.Token)
            thread.Start()
            cts

        let send (name: string) (input: string) =

            use client = new NamedPipeClientStream(".", name, PipeDirection.InOut)

            try
                client.Connect(1000)
                let sw = new StreamWriter(client, Text.Encoding.UTF8, 32767, true)
                sw.Write(input)
                sw.Write('\n')
                sw.Flush()
                sw.Dispose()

                use sr = new StreamReader(client)
                sr.ReadToEnd() |> Some
            with :? TimeoutException ->
                None
