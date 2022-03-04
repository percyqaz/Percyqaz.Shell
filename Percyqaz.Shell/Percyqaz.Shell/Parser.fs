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

    let parse_command, private parse_commandR = createParserForwardedToRef<CommandRequest, unit>()
    let parse_val_expr, private parse_val_exprR = createParserForwardedToRef<Expr, unit>()
    let parse_expr, private parse_exprR = createParserForwardedToRef<Expr, unit>()
    let parse_expr_ext, private parse_expr_extR = createParserForwardedToRef<Expr, unit>()
    let parse_type, private parse_typeR = createParserForwardedToRef<Type, unit>()
    let parse_block_stmt, private parse_block_stmtR = createParserForwardedToRef<Statement, unit>()

    do
    
        let any = stringReturn "any" Type.Any
        let bool = stringReturn "bool" Type.Bool
        let str = stringReturn "str" Type.String
        let num = stringReturn "num" Type.Number
        
        let ws = spaces
        let keyValue = (stringLiteral <|> ident) .>>. (ws >>. pstring ":" >>. ws >>. parse_type)
        let obj = between (pstring "{") (pstring "}") (ws >>. sepBy (keyValue .>> ws) (pstring ";" >>. ws) |>> Map.ofList |>> Type.Object)

        let arr = pstring "array of" >>. spaces1 >>. parse_type |>> Type.Array

        let brackets = between (pchar '(') (pchar ')') parse_type
        
        parse_typeR := choiceL [any; bool; str; num; arr; obj; brackets] "Type"

    do

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
        let jlist = listBetweenStrings "[" "]" parse_expr Expr.Array
        let keyValue = (stringLiteral <|> ident) .>>. (ws >>. pstring ":" >>. ws >>. parse_expr)
        let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> Expr.Object)

        parse_val_exprR := choiceL [jobject; jlist; jstring; jnumber; jtrue; jfalse; jnull] "Value"

    type private Suffix =
        | Prop of string
        | Sub of Expr

    do

        let var = pchar '$' >>. ident |>> Expr.Variable
        let pipe_input = pchar '$' >>% Expr.Pipeline_Variable
        let command = between (pchar '(') (pchar ')') parse_command |>> Expr.Command
        // try catch
        let ternary =
            tuple3 
                (pstring "if" >>. spaces1 >>. parse_expr .>> spaces1)
                (pstring "then" >>. spaces1 >>. parse_expr_ext .>> spaces1)
                (pstring "else" >>. spaces1 >>. parse_expr_ext)
            |>> fun (cond, iftrue, iffalse) -> Expr.Cond ([cond, iftrue], iffalse)

        // suffixes
        let subscript = between (pchar '[') (pchar ']') parse_expr |>> Sub
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
            attempt (spaces >>. pchar '|' >>. spaces >>. (parse_expr_ext >>= pipeline)) |>> fun rest -> Expr.Pipeline (ex, rest)
            <|> preturn ex

        let brackets = between (pchar '(' .>> spaces) (spaces >>. pchar ')') parse_expr_ext

        // blocks
        let block = 
            let linesep = (spaces >>. pchar ';' .>> spaces)
            between (pchar '{' .>> spaces) (spaces >>. pchar '}')
                (
                    (many (attempt (parse_block_stmt .>> linesep)))
                    .>>. parse_expr_ext
                )
            |>> Expr.Block

        parse_exprR := 
            choiceL [attempt var; ternary; pipe_input; parse_val_expr; attempt command; brackets] "Expression"
            >>= suffixes

        parse_expr_extR := 
            attempt ((parse_expr <|> (parse_command |>> Expr.Command)) >>= pipeline)
            <|> block

    type private ReqExFrag =
        | Flag of name: string * value: Expr
        | Arg of Expr

    do
            
        let flag = 
            tuple2
                (pchar '-' >>. ident)
                (opt (attempt (spaces >>. pchar '=' >>. spaces >>. parse_expr)))
            |>> fun (f, v) -> Flag (f, Option.defaultValue (Expr.Bool true) v)

        let arg = parse_expr |>> Arg

        parse_commandR :=
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

    let parse_toplevel_stmt =
        
        let help = (pstring "help" >>. opt (spaces1 >>. ident)) |>> Statement.Help

        let decl =
            tuple3
                (pstring "let" >>. spaces1 >>. ident .>> spaces)
                (opt (pstring ":" >>. spaces >>. parse_type .>> spaces))
                (pstring "=" >>. spaces >>. parse_expr_ext)
            |>> Statement.Declare

        let eval = parse_expr_ext |>> Statement.Eval

        do parse_block_stmtR := choiceL [decl; eval] "Statement"
        
        choiceL [decl; help; eval] "Statement"