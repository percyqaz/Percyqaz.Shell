namespace Percyqaz.Shell

module Parser =
    open FParsec
    open Tree

    let private ident = identifier (new IdentifierOptions())
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

        between (pstring "\"") (pstring "\"") (stringsSepBy normalCharSnippet escapedCharSnippet)

    let private cmdparser, cmdparserRef = createParserForwardedToRef<CommandRequest, unit>()
    let private exparser, exparserRef = createParserForwardedToRef<Expr, unit>()

    let exprForgiveBrackets = exparser <|> (cmdparser |>> Expr.Command)

    let private typeParser =
        let tparser, tparserRef = createParserForwardedToRef<Type, unit>()
    
        let any = stringReturn "any" Type.Any
        let bool = stringReturn "bool" Type.Bool
        let str = stringReturn "str" Type.String
        let num = stringReturn "num" Type.Number
        
        let ws = spaces
        let keyValue = (stringLiteral <|> ident) .>>. (ws >>. pstring ":" >>. ws >>. tparser)
        let obj = between (pstring "{") (pstring "}") (ws >>. sepBy (keyValue .>> ws) (pstring ";" >>. ws) |>> Map.ofList |>> Type.Object)

        let arr = pstring "array of" >>. tparser |>> Type.Array

        let brackets = between (pchar '(') (pchar ')') tparser
        
        do tparserRef := choiceL [any; bool; str; num; arr; obj; brackets] "Type"
        tparser

    let private valueExprParser = 
        let vparser, vparserRef = createParserForwardedToRef<Expr, unit>()

        let jtrue  = stringReturn "true" (Expr.Bool true)
        let jfalse = stringReturn "false" (Expr.Bool false)
        let jnull = stringReturn "null" Expr.Null
        let jnumber = 
            tuple3
                (opt (pstring "-"))
                (many1Satisfy isDigit)
                (opt (pchar '.' >>. many1Satisfy isDigit))
            |>> function
                | (sign, pre, Some post) -> Option.defaultValue "" sign + pre + "." + post
                | (sign, pre, None) -> Option.defaultValue "" sign + pre
            |>> System.Double.TryParse
            |>> fun (s, v) -> Expr.Number v

        let ws = spaces
        let jstring = stringLiteral |>> Expr.String
        let listBetweenStrings sOpen sClose pElement f =
            between (pstring sOpen) (pstring sClose) (ws >>. sepBy (pElement .>> ws) (pstring "," >>. ws) |>> f)
        let jlist = listBetweenStrings "[" "]" exparser Expr.Array
        let keyValue = (stringLiteral <|> ident) .>>. (ws >>. pstring ":" >>. ws >>. exparser)
        let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> Expr.Object)

        //let closure = pchar '@' >>. exparser |>> Expr.Closure

        do vparserRef := choiceL [jobject; jlist; jstring; jnumber; jtrue; jfalse; jnull] "Value"

        vparser

    type private Suffix =
        | Prop of string
        | Sub of Expr

    let exprParser, exprPipeParser =

        let var = pchar '$' >>. ident |>> Expr.Variable
        let pipe_input = pchar '$' >>% Expr.Pipeline_Variable
        let command = between (pchar '(') (pchar ')') cmdparser |>> Expr.Command
        let value = valueExprParser
        // try catch
        let ternary =
            tuple3 
                (pstring "if" >>. spaces1 >>. exparser .>> spaces1)
                (pstring "then" >>. spaces1 >>. exparser .>> spaces1)
                (pstring "else" >>. spaces1 >>. exparser)
            |>> fun (cond, iftrue, iffalse) -> Expr.Cond ([cond, iftrue], iffalse)

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
        let rec pipeline ex =
            attempt (spaces >>. pchar '|' >>. spaces >>. (exprForgiveBrackets >>= pipeline)) |>> fun rest -> Expr.Pipeline (ex, rest)
            <|> preturn ex

        let brackets = between (pchar '(') (pchar ')') exparser

        do exparserRef := 
            choiceL [attempt var; ternary; pipe_input; value; attempt command; brackets] "Expression"
            >>= suffixes

        exparser, exprForgiveBrackets >>= pipeline

    type private ReqExFrag =
        | Flag of name: string * value: Expr
        | Arg of Expr

    let commandParser =
            
        let flag = 
            tuple2
                (pchar '-' >>. ident)
                (opt (attempt (spaces >>. pchar '=' >>. spaces >>. exprParser)))
            |>> fun (f, v) -> Flag (f, Option.defaultValue (Expr.Bool true) v)

        let arg = exprParser |>> Arg

        do cmdparserRef :=
            tuple2 
                ident
                (many (attempt (spaces1 >>. choiceL [flag; arg] "Argument or flag")))
            |>> fun (name, frags) ->
                let flags = frags |> List.choose (function Flag (name, v) -> Some (name, v) | _ -> None) |> Map.ofList
                let args = frags |> List.choose (function Arg ex -> Some ex | _ -> None)
                {
                    Name = name
                    Args = args
                    Flags = flags
                }

        cmdparser

    let stmtParser =
        
        let help = (pstring "help" >>. opt (spaces1 >>. ident)) |>> Statement.Help

        let decl =
            tuple3
                (pstring "let" >>. spaces1 >>. ident .>> spaces)
                (opt (pstring ":" >>. spaces >>. typeParser .>> spaces))
                (pstring "=" >>. spaces >>. exprPipeParser)
            |>> Statement.Declare

        let eval = exprPipeParser |>> Statement.Eval

        choiceL [decl; help; eval] "Statement"