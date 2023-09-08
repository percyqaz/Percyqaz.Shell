namespace Percyqaz.Shell

open System
open Percyqaz.Shell.Data
open FParsec

type IOContext =
    { In: IO.TextReader; Out: IO.TextWriter }
    static member Console = { In = Console.In; Out = Console.Out }

type ShellContext =
    { Commands: Map<string, Func>; IO: IOContext }
    static member Empty = { Commands = Map.empty; IO = IOContext.Console }
    member this.Write(str: string) = this.IO.Out.Write(str)
    member this.WriteLine(str: string) = this.IO.Out.WriteLine(str)
    member this.ReadLine() = this.IO.In.ReadLine()

    member this.WithCommand(name: string, func: Func) = { this with Commands = Map.add name func this.Commands }

    member this.WithCommand (name: string, desc: string, impl: unit -> 'T) =
        this.WithCommand(name, Command.create desc [""] (Impl.Create1 impl))

    member this.WithCommand (name: string, desc: string, arg1name, impl: 'A -> 'T) =
        this.WithCommand(name, Command.create desc [arg1name] (Impl.Create1 impl))

    member this.WithCommand (name: string, desc: string, arg1name, arg2name, impl: 'A -> 'B -> 'T) =
        this.WithCommand(name, Command.create desc [arg1name; arg2name] (Impl.Create2 impl))

    member this.WithCommand (name: string, desc: string, arg1name, arg2name, arg3name, impl: 'A -> 'B -> 'C -> 'T) =
        this.WithCommand(name, Command.create desc [arg1name; arg2name; arg3name] (Impl.Create3 impl))

    member this.WithCommand (name: string, desc: string, arg1name, arg2name, arg3name, arg4name, impl: 'A -> 'B -> 'C -> 'D -> 'T) =
        this.WithCommand(name, Command.create desc [arg1name; arg2name; arg3name; arg4name] (Impl.Create4 impl))

    member this.WithCommand (name: string, desc: string, arg1name, arg2name, arg3name, arg4name, arg5name, impl: 'A -> 'B -> 'C -> 'D -> 'E -> 'T) =
        this.WithCommand(name, Command.create desc [arg1name; arg2name; arg3name; arg4name; arg5name] (Impl.Create5 impl))
        

module Shell =

    type [<RequireQualifiedAccess>] ShellRequest =
        | Help of string option
        | Command of string * Val list
    
    module Parser =
        
        let private stringLiteral =

            let escape =
                anyOf "\"\\/bfnrt" |>> function | 'b' -> "\b" | 'f' -> "\u000C" | 'n' -> "\n" | 'r' -> "\r" | 't' -> "\t" | c -> string c
            let unicodeEscape =
                let hex2int c = (int c &&& 15) + (int c >>> 6) * 9
                pstring "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                    (hex2int h3) * 4096 + (hex2int h2) * 256 + (hex2int h1) * 16 + hex2int h0
                    |> char |> string)
            let escapedCharSnippet = pstring "\\" >>. (escape <|> unicodeEscape)
            let normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\')

            between (pchar '"') (pchar '"') (stringsSepBy normalCharSnippet escapedCharSnippet)

        let parse_text : Parser<Val, unit> =
            stringLiteral
            <|> many1Satisfy (isNoneOf " \t\n")
            |>> Val.Text
            <?> "Text value"

        let parse_number : Parser<Val, unit> =
            tuple2
                (many1Satisfy isDigit)
                (opt (pchar '.' >>. many1Satisfy isDigit))
                |>> function
                | (pre, Some post) -> pre + "." + post
                | (pre, None) -> pre
                |>> System.Double.TryParse
                |>> fun (s, v) -> Val.Num v
            <?> "Number value"

        let parse_nil : Parser<Val, unit> = preturn Val.Nil

        let parse_any = parse_number <|> parse_text

        let rec parse_choices (cases: (string * Type) list) : Parser<Val, unit> =
            match cases with
            | (label, Type.Nil) :: xs ->
                pstringCI label <|> pstringCI ("'" + label) <?> label
                >>% Val.Atom (label, Val.Nil)
                <|> parse_choices xs
            | (label, ty) :: xs -> 
                (
                    (pstringCI label <|> pstringCI ("'" + label)) <?> label
                    >>. spaces >>. pchar '(' >>. spaces >>. parse_by_type ty
                    |>> fun v -> Val.Atom (label, v)
                )
                <|>
                parse_by_type ty |>> fun v -> Val.Atom (label, v)
                <|> parse_choices xs
            | [] -> pzero

        and parse_object (os: Map<string, Type>) =
            let keyValue = 
                os
                |> Map.toSeq
                |> Seq.map (fun (key, ty) -> pstringCI key .>>. (spaces >>. pstring ":" >>. spaces >>. parse_by_type ty))
                |> choice
            pchar '{' >>. spaces >>. sepBy (keyValue .>> spaces) (pchar ',' >>. spaces) .>> pchar '}'
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
            | Type.Array array_ty -> pchar '[' >>. spaces >>. sepBy (parse_by_type array_ty .>> spaces) (pchar ',' >>. spaces) .>> pchar ']' |>> Val.Arr
            | Type.Function _ -> failwithf "Type %O cannot be parsed from command line arguments" ty

        let parse_command (name: string, func: Func) =
            let rec args xs =
                match xs with
                | (_, ty) :: xs -> 
                    spaces >>. parse_by_type ty
                    >>= fun v -> args xs |>> fun rest -> v :: rest
                | [] -> preturn []
            (pstringCI name |>> fun x -> x.ToLower()) .>>. args (fst func.Signature) .>> spaces .>> eof

        let build (ctx: ShellContext) =
            let rec cmds (commands: (string * Func) list) =
                match commands with
                | (name, func) :: xs -> 
                    parse_command (name, func)
                    |>> ShellRequest.Command
                    <|> cmds xs
                | [] -> pzero
            cmds (ctx.Commands |> Map.toList)
            <|> (pstringCI "help" >>. (spaces >>. opt (identifier (IdentifierOptions()))) |>> ShellRequest.Help)
            <?> "The name of a command or 'help'"

    let dispatch (cmd: ShellRequest) (ctx: ShellContext) =
        match cmd with
        | ShellRequest.Command (name, args) ->
            try
                let result = ctx.Commands.[name].Impl args
                if result <> Val.Nil then
                    ctx.WriteLine (result.ToString())
            with err ->
                ctx.WriteLine err.Message
        | ShellRequest.Help None ->
            ctx.WriteLine(sprintf "Available commands: %s" (String.concat ", " (ctx.Commands |> Map.keys)))
        | ShellRequest.Help (Some cmd) ->
            match ctx.Commands.TryFind cmd with
            | None -> ctx.WriteLine (sprintf "No such command '%s'" cmd)
            | Some func ->
                ctx.WriteLine(sprintf "Showing help for '%s':\n" cmd)
    
                ctx.Write("usage: " + cmd)
                for arg, ty in fst func.Signature do
                    if ty <> Type.Nil then ctx.Write(sprintf " <%s: %O>" arg ty)
                let ret = snd func.Signature
                if ret <> Type.Nil then ctx.Write(sprintf " -> %O" ret)
                ctx.WriteLine("\n" + func.Desc)

    let repl (ctx: ShellContext) =
        let parser = Parser.build ctx
        while true do 
            ctx.Write "> "
            match run parser (ctx.ReadLine().Trim()) with
            | Success(res, _, _) -> dispatch res ctx
            | Failure(err, _, _) ->
                let s = err.ToString()
                let s = s.Substring(s.IndexOf('\n') + 1).Replace("Note: The error occurred at the end of the input stream.\r\n", "")
                let s = s.Substring(s.IndexOf('\n') + 1).TrimEnd()
                ctx.WriteLine (sprintf "  %s" s)

    type ShellContext with

        member this.Evaluate(input: string) =
            let parser = Parser.build this
            match run parser (input.Trim()) with
            | Success(res, _, _) -> dispatch res this
            | Failure(err, _, _) ->
                let s = err.ToString()
                let s = s.Substring(s.IndexOf('\n') + 1).Replace("Note: The error occurred at the end of the input stream.\r\n", "   ")
                let s = s.Substring(s.IndexOf('\n') + 1).TrimEnd()
                this.WriteLine (sprintf "  %s" s)

    open System.IO
    open System.IO.Pipes
    open System.Threading

    module IPC =
        
        let start_server (name: string) (ctx: ShellContext) (cancellationToken: CancellationToken) =
            task {
                let server = new NamedPipeServerStream(name, PipeDirection.InOut, 1)

                let reader = new StreamReader(server, Text.Encoding.UTF8)

                let ctx = { ctx with IO = { In = reader; Out = new StreamWriter(server) }}
                let parser = Parser.build ctx
                let BOM = char 65279

                while not cancellationToken.IsCancellationRequested do
                    do! server.WaitForConnectionAsync(cancellationToken)
                    let! request = ctx.IO.In.ReadLineAsync()

                    match run parser (request.Trim().Trim(BOM)) with
                    | Success(res, _, _) -> dispatch res ctx
                    | Failure(err, _, _) ->
                        let s = err.ToString()
                        let s = s.Substring(s.IndexOf('\n') + 1).Replace("Note: The error occurred at the end of the input stream.\r\n", "")
                        let s = s.Substring(s.IndexOf('\n') + 1).TrimEnd()
                        ctx.WriteLine (s)

                    ctx.IO.Out.Flush()

                    server.WaitForPipeDrain()
                    server.Disconnect()

            } |> Async.AwaitTask |> Async.RunSynchronously

        let start_server_thread (name: string) (ctx: ShellContext) =
            let token = new CancellationToken()
            let thread = new Thread(fun () -> start_server name ctx token)
            thread.Start()
            token
        
        let send (name: string) (input: string) =
            
            use client = new NamedPipeClientStream(".", name, PipeDirection.InOut)
            client.Connect()
            let sw = new StreamWriter(client, Text.Encoding.UTF8, 32767, true)
            sw.Write(input)
            sw.Write('\n')
            sw.Flush()
            sw.Dispose()

            use sr = new StreamReader(client)
            sr.ReadToEnd()