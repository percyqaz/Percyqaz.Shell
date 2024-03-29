﻿namespace Percyqaz.Shell

module Parser =

    open FParsec
    open Tree

    let private ident = identifier (new IdentifierOptions(isAsciiLower))
    let private property = identifier (new IdentifierOptions())
    let private modname = identifier (new IdentifierOptions())
    // todo: module names capital only?
    let private stringLiteral =

        let escape =
            anyOf "\"\\/bfnrt"
            |>> function
                | 'b' -> "\b"
                | 'f' -> "\u000C"
                | 'n' -> "\n"
                | 'r' -> "\r"
                | 't' -> "\t"
                | c -> string c

        let unicodeEscape =
            let hex2int c = (int c &&& 15) + (int c >>> 6) * 9

            pstring "u"
            >>. pipe4
                hex
                hex
                hex
                hex
                (fun h3 h2 h1 h0 ->
                    (hex2int h3) * 4096 + (hex2int h2) * 256 + (hex2int h1) * 16 + hex2int h0
                    |> char
                    |> string
                )

        let escapedCharSnippet = pstring "\\" >>. (escape <|> unicodeEscape)
        let normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\')

        between (pstring "\"") (pstring "\"") (stringsSepBy normalCharSnippet escapedCharSnippet)

    let parse_command, private parse_commandR =
        createParserForwardedToRef<string * Arg list, unit> ()

    let parse_val_expr, private parse_val_exprR =
        createParserForwardedToRef<Expr, unit> ()

    let parse_expr, private parse_exprR = createParserForwardedToRef<Expr, unit> ()
    let parse_arg, private parse_argR = createParserForwardedToRef<Arg, unit> ()

    let parse_expr_ext, private parse_expr_extR =
        createParserForwardedToRef<Expr, unit> ()

    let parse_block_stmt, private parse_block_stmtR =
        createParserForwardedToRef<Stmt, unit> ()

    do
        parse_commandR
        := tuple2 ident (many1 (attempt (spaces1 >>. parse_arg))) <?> "Command"

    do

        // SIMPLE VALUES

        let jtrue = stringReturn "True" (Expr.Bool true)
        let jfalse = stringReturn "False" (Expr.Bool false)
        let jnull = stringReturn "Nil" Expr.Nil

        let jnumber =
            tuple2 (many1Satisfy isDigit) (opt (pchar '.' >>. many1Satisfy isDigit))
            |>> function
                | (pre, Some post) -> pre + "." + post
                | (pre, None) -> pre
            |>> System.Double.TryParse
            |>> fun (s, v) -> Expr.Num v

        let jstring = stringLiteral |>> Expr.Str

        let strinterp =
            let frag: Parser<StrFrag, unit> =
                attempt (
                    many1CharsTill (noneOf "`{") (followedBy (pchar '`' <|> pchar '{'))
                    |>> StrFrag.Str
                )
                <|> attempt (pstring "{{" >>% StrFrag.Str "{")
                <|> (between (pchar '{') (pchar '}') parse_expr_ext |>> StrFrag.Ex)

            between (pchar '`') (pchar '`') (many frag) |>> Expr.StrInterp

        let ws = spaces

        let commaSeparatedBetween sOpen sClose pElement f =
            between (pstring sOpen) (pstring sClose) (ws >>. sepBy (pElement .>> ws) (pstring "," >>. ws) |>> f)

        let jlist = commaSeparatedBetween "[" "]" parse_expr_ext Expr.Arr

        let keyValue =
            (stringLiteral <|> property) .>>. (ws >>. pstring ":" >>. ws >>. parse_expr_ext)

        let jobject = commaSeparatedBetween "{" "}" keyValue (Map.ofList >> Expr.Obj)

        parse_val_exprR
        := choiceL [ jobject; jlist; jstring; strinterp; jnumber; jtrue; jfalse; jnull ] "Value"

    type private Suffix =
        | Prop of string
        | Sub of Expr
        | Call of Expr list

    do

        let var = ident |>> Expr.Var
        let modvar = ident .>>. (pchar ':' >>. ident) |>> Expr.ModVar
        let pipe_input = pchar '.' >>% Expr.Pipevar

        let cond =
            let arm token =
                (pstring token >>. spaces1 >>. parse_expr .>> spaces1)
                .>>. (pstring "then" >>. spaces1 >>. parse_expr_ext .>> spaces1)

            tuple3 (arm "if") (many (arm "elif")) (pstring "else" >>. spaces1 >>. parse_expr_ext)
            |>> fun (head, body, basecase) -> Expr.Cond(head :: body, basecase)

        let subscript_suffix =
            between (pchar '[' >>. spaces) (spaces >>. pchar ']') parse_expr_ext |>> Sub

        let property_suffix = pchar '.' >>. property |>> Prop

        let call_suffix =
            between
                (pchar '(' >>. spaces)
                (spaces .>> pchar ')')
                (sepBy parse_expr_ext (spaces >>. pchar ',' >>. spaces))
            |>> Call

        let suffixes ex =
            let rec foldSuffixes ex xs =
                match xs with
                | Prop p :: xs -> foldSuffixes (Expr.Prop(ex, p)) xs
                | Sub s :: xs -> foldSuffixes (Expr.Sub(ex, s)) xs
                | Call args :: xs -> foldSuffixes (Expr.Call(ex, args)) xs
                | [] -> ex

            many (subscript_suffix <|> property_suffix <|> call_suffix) |>> foldSuffixes ex

        let monop =
            choice
                [
                    stringReturn "!" Monop.NOT
                    stringReturn "-" Monop.NEG
                    stringReturn "~" Monop.ROUND
                    stringReturn "#" Monop.LEN
                ]
            .>>. (spaces >>. parse_expr)
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
                )
                <|> preturn left

            let l1 =
                expr_parser
                >>= binop
                        (choice
                            [
                                stringReturn "&&" Binop.AND
                                stringReturn "*" Binop.MUL
                                stringReturn "/" Binop.DIV
                            ]
                         <?> "Logic operator")
                        expr_parser

            let l2 =
                l1
                >>= binop
                        (choice
                            [
                                stringReturn "||" Binop.OR
                                stringReturn "+" Binop.ADD
                                stringReturn "-" Binop.SUB
                            ]
                         <?> "Numeric operator")
                        l1

            let l3 = l2 >>= binop (stringReturn "|" Binop.PIPE <?> "Pipe operator") l2
            l3

        let block =
            let linesep = (spaces >>. pchar ';' .>> spaces)

            between
                (pchar '{' .>> spaces)
                (spaces >>. pchar '}')
                ((many (attempt (parse_block_stmt .>> linesep))) .>>. parse_expr_ext)
            |>> Expr.Block

        parse_exprR
        := choiceL
            [
                cond
                attempt modvar
                attempt var
                pipe_input
                monop
                parse_val_expr
                brackets
            ]
            "Expression"
           >>= suffixes

        parse_argR
        := pchar '$' >>. parse_expr |>> Arg.Expr
           <|> (stringLiteral |>> Arg.Pure)
           <|> (notFollowedByString "then"
                >>. notFollowedByString "elif"
                >>. notFollowedByString "else"
                >>. many1Satisfy (fun c -> isAsciiLetter c || isAnyOf "0123456789_@.:~#!?" c)
                |>> Arg.Pure)

        let apps ex =
            many (attempt (spaces1 >>. parse_arg))
            |>> function
                | [] -> ex
                | xs -> Expr.App(ex, xs)

        parse_expr_extR
        := attempt ((lambda <|> parse_expr) >>= apps |> binops) <|> block <?> "Expression"

    let parse_toplevel_stmt =

        let help =
            attempt (
                pstring "help" >>. spaces1 >>. (modname .>> pstring ":" .>>. ident)
                |>> Stmt.Help_ModuleCmd
            )
            <|> attempt (pstring "help" >>. spaces1 >>. (ident <|> modname) |>> Stmt.Help_ModuleOrCmd)
            <|> (pstring "help" >>% Stmt.Help_All)

        let decl =
            tuple2 (pstring "let" >>. spaces1 >>. ident .>> spaces) (pstring "=" >>. spaces >>. parse_expr_ext)
            |>> Stmt.Decl

        let eval = parse_expr_ext |>> Stmt.Eval

        do parse_block_stmtR := choiceL [ decl; eval ] "Block statement"

        choiceL [ decl; help; eval ] "Top-level statement"

    let parse_script =
        spaces >>. sepBy (parse_block_stmt) (attempt (spaces .>> pchar ';' .>> spaces))
        .>> spaces
        .>> eof
