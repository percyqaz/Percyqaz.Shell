namespace Percyqaz.Shell.deprecated

open System

module rec Tree =

    /// Types. These are constraints on Values to ensure a command is getting only the kind of data it expects
    /// For example, a command that opens a file would only want to take Strings since a file name cannot be an Array
    /// These types are checked before dispatch to prevent such logic errors
    [<RequireQualifiedAccess>]
    type Type =
        | Any
        | String
        | Bool
        | Number
        | Null
        | Object of Map<string, Type>
        | Array of Type
        | Fn of CommandSignature
        override this.ToString() =
            match this with
            | Any -> "any"
            | String -> "str"
            | Bool -> "bool"
            | Number -> "num"
            | Null -> "null"
            | Object ms -> 
                "{ " + 
                ( Map.toSeq ms
                |> Seq.map (fun (prop, ty) -> sprintf "%s: %O" prop ty)
                |> String.concat "; " )
                + " }"
            | Array ty -> sprintf "array of %O" ty
            | Fn sgn ->
                ( sgn.Args
                |> Seq.map (fun (arg, ty) -> sprintf "%O -> " ty)
                |> String.concat "" )
                + 
                ( sgn.OptArgs
                |> Seq.map (fun (arg, ty) -> sprintf "?%O -> " ty)
                |> String.concat "" )
                +
                ( sgn.Flags |> Map.toSeq
                |> Seq.map (fun (flag, ty) -> sprintf "#%s: %O -> " flag ty)
                |> String.concat "" )
                + sgn.ReturnType.ToString()

    /// Values. These are the key building block of the shell
    /// All data is represented by these values
    [<RequireQualifiedAccess>]
    type Val =
        | String of string
        | Number of float
        | Bool of bool
        | Null
        | Object of Map<string, Val>
        | Array of Val list
        | Lambda of signature: CommandSignature * body: ExprC * context: Context
        override this.ToString() =
            match this with
            | String s -> sprintf "%A" s
            | Bool b -> sprintf "%b" b
            | Number n -> sprintf "%O" n
            | Object ms -> 
                "{ " + 
                ( Map.toSeq ms
                |> Seq.map (fun (prop, v) -> sprintf "%s: %O" prop v)
                |> String.concat ", " )
                + " }"
            | Array xs ->
                "[ " + 
                ( xs |> List.map (sprintf "%O") |> String.concat ", " )
                + " ]"
            | Null -> "null"
            | Lambda (s, body, ctx) -> sprintf "<lambda of %O>" s

    /// Signature for a command, specifying what arguments and flags it takes
    type CommandSignature =
        {
            Args: (string * Type) list
            OptArgs: (string * Type) list
            Flags: Map<string, Type>
            ReturnType: Type
        }

    [<AutoOpen>]
    module Syntax =

        /// Expression representations. These are evaluated to Vals during resolution
        type [<RequireQualifiedAccess>] Expr =
            | String of string
            | Number of float
            | Bool of bool
            | Null
            | Object of Map<string, Expr>
            | Array of Expr list
            | Lambda of binds: (string * Type) list * returnType: Type option * body: Expr

            | Pipeline of Expr * rest: Expr
            | Pipeline_Variable
            | Variable of string
            | Subscript of main: Expr * sub: Expr
            | Property of main: Expr * prop: string
            | Command of CommandRequest
            | Cond of arms: (Expr * Expr) list * basecase: Expr
            | Try of Expr * iferror: Expr
            | Block of Statement list * Expr
            | Call_Lambda of Expr * args: Expr list * flags: Map<string, Expr>

        /// Enumeration of possible top-level actions a shell can provide
        type [<RequireQualifiedAccess>] Statement =
            | Declare of string * Type option * Expr
            | Eval of Expr
            | Help of string option

        /// An command call, containing expressions as arguments
        type CommandRequest =
            {
                Name: string
                Args: Expr list
                Flags: Map<string, Expr>
            }

    [<AutoOpen>]
    module Checked =

        type [<RequireQualifiedAccess>] ExprC =
            | String of string
            | Number of float
            | Bool of bool
            | Null
            | Object of Map<string, ExprC>
            | Array of ExprC list
            | Lambda of binds: (string * Type) list * returnType: Type * body: ExprC

            | Pipeline of ExprC * rest: ExprC
            | Pipeline_Variable
            | Variable of string
            | Subscript of main: ExprC * sub: ExprC
            | Property of main: ExprC * prop: string
            | Command of CommandRequestC
            | Cond of arms: (ExprC * ExprC) list * basecase: ExprC
            | Try of ExprC * iferror: ExprC
            | Block of StatementC list * ExprC
            | Call_Lambda of ExprC * args: ExprC list * flags: Map<string, ExprC>

        type [<RequireQualifiedAccess>] StatementC =
            | Declare of string * Type * ExprC
            | Eval of ExprC
            | Help of string option

        type CommandRequestC =
            {
                Name: string
                Args: ExprC list
                Flags: Map<string, ExprC>
            }

    type IOContext =
        {
            In: IO.TextReader
            Out: IO.TextWriter
        }
        static member Default =
            {
                In = Console.In
                Out = Console.Out
            }

    type CommandExecutionContext =
        {
            Args: Val list
            Flags: Map<string, Val>
            IOContext: IOContext
        }

    type CommandInfo =
        {
            Signature: CommandSignature
            Implementation: CommandExecutionContext -> Val
        }
    
    type Context =
        {
            Variables: Map<string, Type * Val>
            Commands: Map<string, CommandInfo>
            IO: IOContext
        }
        static member Empty =
            {
                Variables = Map.empty
                Commands = Map.empty
                IO = IOContext.Default
            }
        member this.Write(str: string) = this.IO.Out.Write(str)
        member this.WriteLine(str: string) = this.IO.Out.WriteLine(str)
        member this.ReadLine() : string = this.IO.In.ReadLine()
        member this.WithPipelineType(ty: Type) = { this with Variables = Map.add "" (ty, Val.Null) this.Variables }
        member this.WithPipelineValue(value: Val) = { this with Variables = Map.add "" (Type.Any, value) this.Variables }
        member this.WithVarType(name: string, ty: Type) = { this with Variables = Map.add name (ty, Val.Null) this.Variables }
        member this.WithVar(name: string, value: Val, ty: Type) = { this with Variables = Map.add name (ty, value) this.Variables }