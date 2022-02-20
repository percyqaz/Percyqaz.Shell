namespace Percyqaz.Shell

module Parser =
    open FParsec
    open Tree

    let private ident = identifier (new IdentifierOptions())

    let private cmdparser, cmdparserRef = createParserForwardedToRef<CommandRequestEx, unit>()
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