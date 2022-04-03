namespace Percyqaz.Shell

open System

module Tree =

    type Func =
        {
            // For documentation/help purposes
            Binds: string list
            Desc: string
            // Only this implementation matters
            Impl: Val list -> Val
        }
    
    and [<RequireQualifiedAccess>] Val =
        | Str of string
        | Num of float
        | Bool of bool
        | Nil

        | Obj of Map<string, Val>
        | Arr of Val list
        | Func of Func
        override this.ToString() =
            match this with
            | Str s -> s
            | Num n -> sprintf "%O" n
            | Bool b -> if b then "True" else "False"
            | Nil -> "Nil"

            | Obj ms -> 
                "{ " + 
                ( Map.toSeq ms
                |> Seq.map (fun (prop, v) -> sprintf "%s: %O" prop v)
                |> String.concat ", " )
                + " }"
            | Arr xs ->
                "[" + 
                ( xs |> List.map (sprintf "%O") |> String.concat ", " )
                + "]"
            | Func f -> sprintf "<function of arity %i>" f.Binds.Length

    and [<RequireQualifiedAccess>] Monop =
        | ECHO
        | STR
        | TRUTH
        | NOT
        | NEG
        | ROUND
        override this.ToString() =
            match this with
            | ECHO -> "@:"
            | STR -> "@"
            | TRUTH -> "?"
            | NOT -> "!"
            | NEG -> "-"
            | ROUND -> "~"

    and [<RequireQualifiedAccess>] Binop =
        | OR
        | PIPE
        | AND
        | ADD
        | SUB
        | MUL
        | DIV
        override this.ToString() =
            match this with
            | OR -> "||"
            | PIPE -> "|"
            | AND -> "&&"
            | ADD -> "+"
            | SUB -> "-"
            | MUL -> "*"
            | DIV -> "/"
        
    and [<RequireQualifiedAccess>] StrFrag =
        | Ex of Expr
        | Str of string

    and [<RequireQualifiedAccess>] Expr =
        | Str of string
        | StrInterp of StrFrag list
        | Num of float
        | Bool of bool
        | Nil
        | Obj of Map<string, Expr>
        | Arr of Expr list
        | Func of binds: string list * body: Expr

        | Monop of op: Monop * ex: Expr
        | Binop of op: Binop * left: Expr * right: Expr
        | Pipevar
        | Var of string
        | Sub of main: Expr * sub: Expr
        | Prop of main: Expr * prop: string
        | Cond of arms: (Expr * Expr) list * basecase: Expr

        | Block of Stmt list * Expr
        | Cmd of id: string * args: Expr list
        | VarCall of Expr * args: Expr list
        override this.ToString() =
            match this with
            | Str s -> sprintf "%A" s
            | StrInterp xs ->
                List.map (function StrFrag.Ex ex -> sprintf "{%O}" ex | StrFrag.Str s -> s) xs
                |> String.concat "" |> sprintf "%A"
            | Num n -> n.ToString() // culture invariant
            | Bool b -> if b then "True" else "False"
            | Nil -> "Nil"
            | Obj ms -> 
                "{ " + 
                ( Map.toSeq ms
                |> Seq.map (fun (prop, v) -> sprintf "%s: %O" prop v)
                |> String.concat ", " )
                + " }"
            | Arr xs ->
                "[" + 
                ( xs |> List.map (sprintf "%O") |> String.concat ", " )
                + "]"
            | Func (binds, _) -> binds |> String.concat ", " |> sprintf "|%s| -> ..."

            | Monop (op, ex) -> sprintf "%O%O" op ex
            | Binop (op, left, right) -> sprintf "(%O %O %O)" left op right
            | Pipevar -> "$"
            | Var v -> sprintf "$%s" v
            | Sub (main, sub) -> sprintf "%O[%O]" main sub
            | Prop (main, prop) -> sprintf "%O.%s" main prop
            | Cond (_, basecase) -> sprintf "if ... else %O" basecase
            
            | Block (_, ex) -> sprintf "{ ... ; %O }" ex
            | Cmd (id, _) -> sprintf "%s ..." id
            | VarCall (ex, _) -> sprintf "%O(...)" ex

    and [<RequireQualifiedAccess>] Stmt =
        | Decl of string * Expr
        | Eval of Expr
        | Help of string option

    and IOContext =
        {
            In: IO.TextReader
            Out: IO.TextWriter
        }
        static member Default =
            {
                In = Console.In
                Out = Console.Out
            }

    and Context =
        {
            Vars: Map<string, Val>
            IO: IOContext
        }
        static member Empty =
            {
                Vars = Map.empty
                IO = IOContext.Default
            }
        member this.Write(str: string) = this.IO.Out.Write(str)
        member this.WriteLine(str: string) = this.IO.Out.WriteLine(str)
        member this.ReadLine() : string = this.IO.In.ReadLine()
        
        member this.WithVar(name: string, value: Val) = { this with Vars = Map.add name value this.Vars }
        member this.WithPipeVar(value: Val) = this.WithVar("", value)
        member this.WithCommand(name: string, func: Func) = this.WithVar(name, Val.Func func)