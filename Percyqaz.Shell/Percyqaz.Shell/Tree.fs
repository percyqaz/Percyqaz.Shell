namespace Percyqaz.Shell

open System

module Tree =

    /// Types. These are constraints on Values to ensure a command is getting only the kind of data it expects
    /// For example, a command that opens a file would only want to take Strings since a file name cannot be an Array
    /// These types are checked before dispatch to prevent such logic errors
    [<RequireQualifiedAccess>]
    type Type =
        | Any
        | String
        | Bool
        | Number
        | Object of Map<string, Type>
        | Array of Type
        | Null
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
                |> String.concat "; " )
                + " }"
            | Array ty -> sprintf "%O[]" ty
            | Null -> "Null"

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
        override this.ToString() =
            match this with
            | String s -> sprintf "%A" s
            | Bool b -> sprintf "%b" b
            | Number n -> sprintf "%f" n
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

    /// Expression representations. These are evaluated to Vals during resolution
    and [<RequireQualifiedAccess>] Expr =
        | String of string
        | Number of float
        | Bool of bool
        | Null
        | Object of Map<string, Expr>
        | Array of Expr list

        | Piped_Input
        | Variable of string
        | Subscript of main: Expr * sub: Expr
        | Property of main: Expr * prop: string
        | Evaluate_Command of CommandRequest
        | Cond of condition: Expr * iftrue: Expr * iffalse: Expr
        | Try of Expr * iferror: Expr

    and [<RequireQualifiedAccess>] Statement =
        | Declare of string * Type option * Expr
        | Command of CommandRequest
        | Help of string option

    /// An unresolved command request, containing expressions as arguments.
    /// Converted to a CommandRequest via resolution
    and CommandRequest =
        {
            Name: string
            Args: Expr list
            Flags: Map<string, Expr>
        }

    /// Signature for a command, specifying what arguments and flags it takes
    type CommandSignature =
        {
            Args: (string * Type) list
            OptArgs: (string * Type) list
            Flags: Map<string, Type>
            ReturnType: Type
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