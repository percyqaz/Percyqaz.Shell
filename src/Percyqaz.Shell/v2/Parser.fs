namespace Percyqaz.Shell.v2

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
        
    let parse_command, private parse_commandR = createParserForwardedToRef<string * Expr list, unit>()
    let parse_val_expr, private parse_val_exprR = createParserForwardedToRef<Expr, unit>()
    let parse_expr, private parse_exprR = createParserForwardedToRef<Expr, unit>()
    let parse_expr_ext, private parse_expr_extR = createParserForwardedToRef<Expr, unit>()
    let parse_block_stmt, private parse_block_stmtR = createParserForwardedToRef<Stmt, unit>()

    do parse_commandR := tuple2 ident (many (attempt (spaces1 >>. parse_expr)))

    do

        let jtrue  = stringReturn "True" (Expr.Bool true)
        let jfalse = stringReturn "False" (Expr.Bool false)
        let jnull = stringReturn "Nil" Expr.Nil
        let jnumber = 
            tuple2
                (many1Satisfy isDigit)
                (opt (pchar '.' >>. many1Satisfy isDigit))
            |>> function
                | (pre, Some post) -> pre + "." + post
                | (pre, None) -> pre
            |>> System.Double.TryParse
            |>> fun (s, v) -> Expr.Num v

        let ws = spaces
        let jstring = stringLiteral |>> Expr.Str
        let listBetweenStrings sOpen sClose pElement f =
            between (pstring sOpen) (pstring sClose) (ws >>. sepBy (pElement .>> ws) (pstring "," >>. ws) |>> f)
        let jlist = listBetweenStrings "[" "]" parse_expr Expr.Arr
        let keyValue = (stringLiteral <|> ident) .>>. (ws >>. pstring ":" >>. ws >>. parse_expr)
        let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> Expr.Obj)

        parse_val_exprR := choiceL [jobject; jlist; jstring; jnumber; jtrue; jfalse; jnull] "Value"

    type private Suffix =
        | Prop of string
        | Sub of Expr
        | Call of Expr list

    do

        let var = pchar '$' >>. ident |>> Expr.Var
        let pipe_input = pchar '$' >>% Expr.Pipevar
        let cond =
            let arm token =
                (pstring token >>. spaces1 >>. parse_expr .>> spaces1)
                .>>. (pstring "then" >>. spaces1 >>. parse_expr_ext .>> spaces1)
            tuple3 
                (arm "if")
                (many (arm "elif"))
                (pstring "else" >>. spaces1 >>. parse_expr_ext)
            |>> fun (head, body, basecase) -> Expr.Cond (head :: body, basecase)

        // suffixes
        let subscript = between (pchar '[' >>. spaces) (spaces >>. pchar ']') parse_expr_ext |>> Sub
        let property = pchar '.' >>. ident |>> Prop
        let call = between (pchar '(' >>. spaces) (spaces .>> pchar ')') (sepBy parse_expr_ext (spaces >>. pchar ',' >>. spaces)) |>> Call
        let suffixes ex =
            let rec foldSuffixes ex xs =
                match xs with
                | Prop p :: xs -> foldSuffixes (Expr.Prop (ex, p)) xs
                | Sub s :: xs -> foldSuffixes (Expr.Sub(ex, s)) xs
                | Call args :: xs -> foldSuffixes (Expr.VarCall(ex, args)) xs
                | [] -> ex
            many (subscript <|> property <|> call)
            |>> foldSuffixes ex

        let monop =
            choice [
                stringReturn "@:" Monop.ECHO
                stringReturn "@" Monop.STR
                stringReturn "?" Monop.TRUTH
                stringReturn "!" Monop.NOT
                stringReturn "-" Monop.NEG
                stringReturn "~" Monop.ROUND
            ] .>>.
            (spaces >>. parse_expr)
            |>> Expr.Monop

        let brackets = between (pchar '(' .>> spaces) (spaces >>. pchar ')') parse_expr_ext

        let lambda =
              let sep = spaces >>. pchar ',' >>. spaces
              tuple2
                  (pchar '|' >>. spaces >>. sepBy ident sep .>> spaces .>> pchar '|')
                  (spaces >>. pstring "->" >>. spaces >>. parse_expr_ext)
              |>> Expr.Func

        let binops expr_parser =

            let rec binop ops p left =
                attempt (
                    spaces >>. ops .>>. (spaces >>. p)
                    |>> fun (op, rest) -> Expr.Binop(op, left, rest)
                    >>= binop ops p
                ) <|> preturn left

            let l1 = 
                expr_parser
                >>= binop (choice [
                    stringReturn "&&" Binop.AND
                    stringReturn "*" Binop.MUL
                    stringReturn "/" Binop.DIV
                ]) expr_parser
            let l2 =
                l1
                >>= binop (choice [
                    stringReturn "||" Binop.OR
                    stringReturn "+" Binop.ADD
                    stringReturn "-" Binop.SUB
                ]) l1
            let l3 = l2 >>= binop (stringReturn "|" Binop.PIPE) l2
            l3

        let block = 
            let linesep = (spaces >>. pchar ';' .>> spaces)
            between (pchar '{' .>> spaces) (spaces >>. pchar '}')
                (
                    (many (attempt (parse_block_stmt .>> linesep)))
                    .>>. parse_expr_ext
                )
            |>> Expr.Block

        parse_exprR := 
            choiceL [attempt var; cond; pipe_input; monop; lambda; parse_val_expr; brackets] "Expression"
            >>= suffixes

        parse_expr_extR := 
            attempt ((parse_expr <|> (parse_command |>> Expr.Cmd)) |> binops)
            <|> block

    let parse_toplevel_stmt =
        
        let help = (pstring "help" >>. opt (spaces1 >>. ident)) |>> Stmt.Help

        let decl =
            tuple2
                (pstring "let" >>. spaces1 >>. pchar '$' >>. ident .>> spaces)
                (pstring "=" >>. spaces >>. parse_expr_ext)
            |>> Stmt.Decl

        let eval = parse_expr_ext |>> Stmt.Eval

        do parse_block_stmtR := choiceL [decl; eval] "Statement"
        
        choiceL [decl; help; eval] "Statement"