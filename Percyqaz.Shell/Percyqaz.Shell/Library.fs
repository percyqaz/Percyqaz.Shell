namespace Percyqaz.Shell

open System

module Tree =

    [<RequireQualifiedAccess>]
    type Type =
        | Any
        | String
        | Bool
        | Number
        | Object of Map<string, Type>
        | Array of Type
        | Unit
        | Closure // in future can have a whole function signature
        override this.ToString() =
            match this with
            | Any -> "Any"
            | String -> "String"
            | Bool -> "Bool"
            | Number -> "Number"
            | Object ms -> 
                "{ " + 
                ( Map.toSeq ms
                |> Seq.map (fun (prop, ty) -> sprintf "%s: %O" prop ty)
                |> String.concat ", " )
                + " }"
            | Array ty -> sprintf "%O[]" ty
            | Unit -> "Unit"
            | Closure -> "Closure"

    [<RequireQualifiedAccess>]
    type Val =
        | String of string
        | Number of float
        | Bool of bool
        | Unit
        | Object of Map<string, Val>
        | Array of Val list
        | Closure of Req
        override this.ToString() =
            match this with
            | String s -> sprintf "%A" s
            | Bool b -> sprintf "%b" b
            | Number n -> sprintf "%f" n
            | Object ms -> 
                "{ " + 
                ( Map.toSeq ms
                |> Seq.map (fun (prop, v) -> sprintf "%s = %O" prop v)
                |> String.concat ", " )
                + " }"
            | Array xs ->
                "[ " + 
                ( xs |> List.map (sprintf "%O") |> String.concat ", " )
                + " ]"
            | Unit -> "()"
            | Closure req -> "<Closure @0x5E1B008>"
        
    and [<RequireQualifiedAccess>] ValEx =
        | String of string
        | Number of float
        | Bool of bool
        | Unit
        | Object of Map<string, Expr>
        | Array of Expr list
        | Closure of Expr

    and [<RequireQualifiedAccess>] Expr =
        | Piped_Input
        | Variable of string
        | Subscript of main: Expr * sub: Expr
        | Property of main: Expr * prop: string
        | Evaluate_Command of ReqEx
        | Val of ValEx
        | Cond of condition: Expr * iftrue: Expr * iffalse: Expr
        | Try of Expr * iferror: Expr
    
    and Req =
        {
            Name: string
            Args: Val list
            Flags: Map<string, Val>
        }

    and ReqEx =
        {
            Name: string
            Args: Expr list
            Flags: Map<string, Expr>
        }

    type Sig =
        {
            Args: (string * Type) list
            OptArgs: (string * Type * Val) list
            Flags: Map<string, Type * Val>
        }

    type Cmd =
        {
            Name: string
            Sig: Sig
            Impl: Req -> Val
        }

    module Impl =
        
        let usage (i: Cmd) =
            printfn "usage of '%s':\n" i.Name
            printf " %s" i.Name
            for (name, t) in i.Sig.Args do
                printf " <%s>" name
            for (name, t, d) in i.Sig.OptArgs do
                printf " [%s]" name
            printfn "\n"
            printfn " flags: "
            for KeyValue (name, (t, d)) in i.Sig.Flags do
                printfn "  -%s (%O, default is %O)" name t d
            printfn " --"
            for (name, t) in i.Sig.Args do
                printf " %s is a %O" name t
            for (name, t, d) in i.Sig.OptArgs do
                printf " %s is a %O (default is %O)" name t d
        
    [<RequireQualifiedAccess>]
    type Err =
        | NotFound of string
        | Mismatch of string list
        | Unhandled of Exception

    type ReqResult = Result<Val, Err>

module Parser =
    open FParsec
    open Tree

    let private ident = identifier (new IdentifierOptions())

    let private cmdparser, cmdparserRef = createParserForwardedToRef<ReqEx, unit>()
    let private exparser, exparserRef = createParserForwardedToRef<Expr, unit>()

    let valueParser = 
        let vparser, vparserRef = createParserForwardedToRef<ValEx, unit>()

        let jtrue  = stringReturn "Y" (ValEx.Bool true)
        let jfalse = stringReturn "N" (ValEx.Bool false)
        let jnull = stringReturn "()" ValEx.Unit
        let jnumber = 
            (many1Satisfy isDigit) .>>. opt (pchar '.' >>. many1Satisfy isDigit)
            |>> function
                | (pre, Some post) -> pre + "." + post
                | (pre, None) -> pre
            |>> System.Double.TryParse
            |>> fun (s, v) -> ValEx.Number v

        let str s = pstring s
        let stringLiteral =

            let escape =
                anyOf "\"\\/bfnrt" |>> function | 'b' -> "\b" | 'f' -> "\u000C" | 'n' -> "\n" | 'r' -> "\r" | 't' -> "\t" | c -> string c
            let unicodeEscape =
                let hex2int c = (int c &&& 15) + (int c >>> 6) * 9
                str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                    (hex2int h3) * 4096 + (hex2int h2) * 256 + (hex2int h1) * 16 + hex2int h0
                    |> char |> string)
            let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
            let normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\')

            between (str "\"") (str "\"") (stringsSepBy normalCharSnippet escapedCharSnippet)

        let ws = spaces
        let jstring = stringLiteral |>> ValEx.String
        let listBetweenStrings sOpen sClose pElement f =
            between (str sOpen) (str sClose) (ws >>. sepBy (pElement .>> ws) (str "," >>. ws) |>> f)
        let jlist = listBetweenStrings "[" "]" exparser ValEx.Array
        let keyValue = (stringLiteral <|> ident) .>>. (ws >>. str ":" >>. ws >>. exparser)
        let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> ValEx.Object)

        let closure = pchar '@' >>. exparser |>> ValEx.Closure

        do vparserRef := choiceL [jobject; jlist; closure; jstring; jnumber; jtrue; jfalse; jnull] "Value"

        vparser

    type private Suffix =
        | Prop of string
        | Sub of Expr

    let exprParser =

        let var = pchar '$' >>. ident |>> Expr.Variable
        let pipe_input = pchar '$' >>% Expr.Piped_Input
        let command = between (pchar '(') (pchar ')') cmdparser |>> Expr.Evaluate_Command
        let value = valueParser |>> Expr.Val
        // try catch
        let ternary =
            tuple3 
                (pstring "if" >>. spaces1 >>. exparser .>> spaces1)
                (pstring "then" >>. spaces1 >>. exparser .>> spaces1)
                (pstring "else" >>. spaces1 >>. exparser)
            |>> fun (cond, iftrue, iffalse) -> Expr.Cond (cond, iftrue, iffalse)

        // suffixes
        let subscript = between (pchar '[') (pchar ']') exparser |>> Sub
        let property = pchar '.' >>. ident |>> Prop
        let suffixes ex =
            let rec foldSuffixes ex xs =
                match xs with
                | Prop p :: xs -> foldSuffixes (Expr.Property (ex, p)) xs
                | Sub s :: xs -> foldSuffixes (Expr.Subscript(ex, s)) xs
                | [] -> ex
            many (subscript <|> property)
            |>> foldSuffixes ex

        let brackets = between (pchar '(') (pchar ')') exparser

        do exparserRef := 
            choiceL [attempt var; ternary; pipe_input; value; attempt command; brackets] "Expression"
            >>= suffixes

        exparser

    type private ReqExFrag =
        | Flag of name: string * value: Expr
        | Arg of Expr

    let commandParser =
            
        let flag = 
            tuple2
                (pchar '-' >>. ident)
                (opt (attempt (spaces >>. pchar '=' >>. spaces >>. exprParser)))
            |>> fun (f, v) -> Flag (f, Option.defaultValue (Expr.Val (ValEx.Bool true)) v)

        let arg = exprParser |>> Arg

        do cmdparserRef :=
            tuple2 
                ident
                (many (spaces1 >>. choiceL [flag; arg] "Argument or flag"))
            |>> fun (name, frags) ->
                let flags = frags |> List.choose (function Flag (name, v) -> Some (name, v) | _ -> None) |> Map.ofList
                let args = frags |> List.choose (function Arg ex -> Some ex | _ -> None)
                {
                    Name = name
                    Args = args
                    Flags = flags
                }

        cmdparser

module Helpers =
    
    open Tree

    let rec type_check (t: Type) (v: Val) : Result<unit, string list> =
        match t, v with
        | Type.Any, _ -> Ok()
        | Type.String, Val.String _ -> Ok()
        | Type.Bool, Val.Bool _ -> Ok()
        | Type.Number, Val.Number _ -> Ok()
        | Type.Object ms, Val.Object vs ->
            let errors =
                Seq.fold 
                    ( fun errs (KeyValue (prop, propType)) ->
                        if vs.ContainsKey(prop) then
                            match type_check propType vs.[prop] with
                            | Ok _ -> errs
                            | Error es -> (List.map (fun s -> sprintf "Property '%s': %s" prop s) es) @ errs
                        else sprintf "Missing property '%s': Expected a %O" prop propType :: errs
                    )
                    [] ms
            if errors.IsEmpty then Ok() else Error errors
        | Type.Array ty, Val.Array xs ->
            let errors =
                List.fold
                    ( fun errs (i, x) ->
                        match type_check ty x with
                        | Ok _ -> errs
                        | Error es -> (List.map (fun s -> sprintf "Element %i: %s" i s) es) @ errs
                    )
                    [] (List.indexed xs)
            if errors.IsEmpty then Ok() else Error errors
        | Type.Unit, Val.Unit -> Ok()
        | Type.Closure, Val.Closure _ -> Ok()
        | _ -> Error [sprintf "Expected a %O, got %O" t v]

    let rec type_check_req (cmd: Cmd) (req: Req) : Result<unit, string list> =
        let mutable args = req.Args
        let mutable errors = []

        for (name, ty) in cmd.Sig.Args do
            match args with
            | x :: xs ->
                match type_check ty x with
                | Ok() -> ()
                | Error errs -> errors <- (List.map (fun s -> sprintf "Argument '%s': %s" name s) errs) @ errors
                args <- xs
            | [] -> errors <- sprintf "Missing argument '%s': Expected a %O" name ty :: errors

        for (name, ty, _) in cmd.Sig.OptArgs do
            match args with
            | x :: xs ->
                match type_check ty x with
                | Ok() -> ()
                | Error errs -> errors <- (List.map (fun s -> sprintf "Optional argument '%s': %s" name s) errs) @ errors
                args <- xs
            | [] -> ()

        while args <> [] do
            errors <- sprintf "Unexpected argument %O" (List.head args) :: errors
            args <- List.tail args

        for KeyValue (name, v) in req.Flags do
            if cmd.Sig.Flags.ContainsKey name then
                match type_check (fst cmd.Sig.Flags.[name]) v with
                | Ok() -> ()
                | Error errs -> errors <- (List.map (fun s -> sprintf "Flag '%s': %s" name s) errs) @ errors
            else errors <- sprintf "Unrecognised flag '%s'" name :: errors
        if errors.IsEmpty then Ok() else Error errors

module Library =

    open Tree
    open FParsec

    type Context =
        {
            Variables: Map<string, Val>
            Commands: Cmd list
            PipedVal: Val
        }
        static member Empty =
            {
                Variables = Map.empty
                Commands =
                    [
                        {
                            Name = "echo"
                            Sig =
                                {
                                    Args = [("input", Type.Any)]
                                    OptArgs = []
                                    Flags = Map.empty
                                }
                            Impl = fun req -> printfn "%O" req.Args.[0]; Val.Unit
                        }
                    ]
                PipedVal = Val.Unit
            }

    module Context =

        let rec eval_valx (vx: ValEx) (ctx: Context) : Val =
            match vx with
            | ValEx.String s -> Val.String s
            | ValEx.Number n -> Val.Number n
            | ValEx.Bool b -> Val.Bool b
            | ValEx.Unit -> Val.Unit
            | ValEx.Object m -> m |> Map.map (fun _ ex -> eval_expr ex ctx) |> Val.Object
            | ValEx.Array xs -> xs |> List.map (fun ex -> eval_expr ex ctx) |> Val.Array
            | ValEx.Closure x ->
                match eval_expr x ctx with
                | Val.Closure req -> Val.Closure req
                | x -> failwithf "Expected a closure here but got %A" x

        and eval_expr (ex: Expr) (ctx: Context) =
            match ex with
            | Expr.Piped_Input -> ctx.PipedVal
            | Expr.Variable x -> if ctx.Variables.ContainsKey x then ctx.Variables.[x] else failwithf "Unrecognised variable: '%s'" x
            | Expr.Subscript (main, sub) -> 
                let m = eval_expr main ctx
                let s = eval_expr sub ctx
                match m with
                | Val.Array xs ->
                    match s with
                    | Val.Number f -> xs.[int f]
                    | _ -> failwith "Must subscript arrays with a number"
                | Val.Object ms ->
                    match s with
                    | Val.String s -> if ms.ContainsKey s then ms.[s] else failwithf "Object has no such property '%s'" s
                    | _ -> failwith "Must subscript objects with a string"
                | _ -> failwith "This value is not subscriptable"
            | Expr.Property (main, prop) ->
                let m = eval_expr main ctx
                match m with
                | Val.Object ms ->
                    if ms.ContainsKey prop then ms.[prop] else failwithf "Object has no such property '%s'" prop
                | _ -> failwith "This value is not an object"
            | Expr.Evaluate_Command (rx: ReqEx) ->
                let req = resolve_req rx ctx
                match dispatch_req req ctx with
                | Result.Ok v -> v
                | Result.Error err -> failwith "Error dispatching subcommand" // needs more
            | Expr.Val (v: ValEx) -> eval_valx v ctx
            | Expr.Cond (cond, iftrue, iffalse) ->
                let c = eval_expr cond ctx
                if (true) then //truthiness nyi
                    eval_expr iftrue ctx
                else eval_expr iffalse ctx
            | Expr.Try (ex, iferror) -> failwith "nyi needs some infrastructure"

        and resolve_req (req: ReqEx) (ctx: Context) : Req =
            {
                Name = req.Name
                Args = req.Args |> List.map (fun ex -> eval_expr ex ctx)
                Flags = req.Flags |> Map.map (fun _ ex -> eval_expr ex ctx)
            }

        and dispatch_req (req: Req) (ctx: Context) : ReqResult =
            match List.tryFind (fun i -> i.Name = req.Name) ctx.Commands with
            | Some cmd ->
                match Helpers.type_check_req cmd req with
                | Result.Ok() -> 
                    try cmd.Impl req |> ReqResult.Ok
                    with err -> ReqResult.Error (Err.Unhandled err)
                | Result.Error errs -> ReqResult.Error (Err.Mismatch errs)
            | None -> ReqResult.Error (Err.NotFound (sprintf "Unrecognised command '%s'" req.Name))
            

    type Context with
        member this.Evaluate(command: ReqEx) = Context.resolve_req command this
        member this.Evaluate(expr: Expr) = Context.eval_expr expr this

        member this.Mainloop() =
            while true do
                Console.ReadLine()
                |> run Parser.commandParser
                |> 
                    function
                    | Success (res, _, _) ->
                        try
                            let req = Context.resolve_req res this
                            match Context.dispatch_req req this with
                            | Result.Ok v -> printfn "OK: %O" v
                            | Result.Error e -> printfn "RUNFAIL: %A" e
                        with err -> printfn "RESFAIL: %A" err 
                    | Failure (err, _, _) -> printfn "PARSEFAIL: %A" err