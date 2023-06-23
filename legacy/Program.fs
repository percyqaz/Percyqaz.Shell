open FParsec

type Token =
    | LET

    | EQUALS
    | PLUS
    | MINUS
    | STAR

    | DOUBLE_COLON
    | COLON
    | BAR
    | DOT
    | BRA
    | KET
    | A_BRA
    | A_KET
    | C_BRA
    | C_KET
    | S_BRA
    | S_KET

    | Atom of string
    | Ident of string
    | Name of string

    | INDENT
    | DEDENT
    | NEWLINE
    | BAD_DEDENT

let sample =
    """in x : Int
let f (x: String) :: String =
    x + x + y

f(x) * x * x

type Cases = '1 of Int | '2 of String | '3 of Bool
let f(c: Cases)
	match 
		c
	with
	  '1 as i -> return i + 5
	  '2 as s -> return String.length s
	typeof(c) // '3 of Bool"""

let ident = many1Satisfy2 isAsciiLower (isAnyOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_") |>> Ident
let atom = skipChar '\'' >>. many1Satisfy (isAnyOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_") |>> Atom
let name = many1Satisfy2 isAsciiUpper (isAnyOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_") |>> Name

let indentation =
    getUserState
    >>= fun known_indents ->

        lookAhead (pchar '\n' >>. manySatisfy (isAnyOf " \t"))
        >>= fun got_indent ->

            let consume_indent = skipChar '\n' >>. skipString got_indent

            if got_indent.Length > List.head known_indents then
                setUserState (got_indent.Length :: known_indents) >>. consume_indent >>. preturn INDENT
            elif got_indent.Length = List.head known_indents then
                consume_indent >>. preturn NEWLINE
            elif got_indent.Length > known_indents.[1] then
                consume_indent >>. preturn BAD_DEDENT
            elif got_indent.Length = known_indents.[1] then
                setUserState (List.tail known_indents) >>. consume_indent >>. preturn DEDENT
            else
                setUserState (List.tail known_indents) >>. preturn DEDENT

let token = 
        (skipString "let" >>% LET)

    <|> (skipString "=" >>% EQUALS)
    <|> (skipString "+" >>% PLUS)
    <|> (skipString "-" >>% MINUS)
    <|> (skipString "*" >>% STAR)
    <|> (skipString "::" >>% DOUBLE_COLON)
    <|> (skipString ":" >>% COLON)
    <|> (skipString "|" >>% BAR)
    <|> (skipString "." >>% DOT)
    <|> (skipString "(" >>% BRA)
    <|> (skipString ")" >>% KET)
    <|> (skipString "<" >>% A_BRA)
    <|> (skipString ">" >>% A_KET)
    <|> (skipString "{" >>% C_BRA)
    <|> (skipString "}" >>% C_KET)
    <|> (skipString "[" >>% S_BRA)
    <|> (skipString "]" >>% S_KET)

    <|> name
    <|> atom
    <|> ident
    <|> indentation
let spacing = many1Satisfy (fun c -> c = ' ')

runParserOnString (many (token .>> optional spacing)) [0] "Test data" sample
|> printfn "%A"

open Percyqaz.Shell
open Percyqaz.Shell.Data

ShellContext.Empty
    .WithCommand("echo",
        "Echoes its input back",
        ["message"],
        printfn "%s")
    .WithCommand("sum",
        "Sums a list of numbers",
        ["xs"],
        fun (xs: float list) -> List.sum xs)
    .WithCommand("not",
        "Not function",
        ["flag"],
        not)
|> Shell.repl