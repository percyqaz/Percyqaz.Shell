namespace Percyqaz.Shell

open System

module Tree =

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
        | Closure of CommandRequest
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
            | Null -> "null"
            | Closure req -> "<Closure @0x5E1B008>"

    /// Expression representations. These are evaluated to Vals during resolution
    and [<RequireQualifiedAccess>] Expr =
        | String of string
        | Number of float
        | Bool of bool
        | Null
        | Object of Map<string, Expr>
        | Array of Expr list
        | Closure of Expr

        | Piped_Input
        | Variable of string
        | Subscript of main: Expr * sub: Expr
        | Property of main: Expr * prop: string
        | Evaluate_Command of CommandRequestEx
        | Cond of condition: Expr * iftrue: Expr * iffalse: Expr
        | Try of Expr * iferror: Expr
    
    /// A resolved command request, ready for check + dispatch
    and CommandRequest =
        {
            Name: string
            Args: Val list
            Flags: Map<string, Val>
        }

    /// An unresolved command request, containing expressions as arguments.
    /// Converted to a CommandRequest via resolution
    and CommandRequestEx =
        {
            Name: string
            Args: Expr list
            Flags: Map<string, Expr>
        }
    
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
            | Null -> "Null"
            | Closure -> "Closure"

    /// Signature for a command, specifying what arguments and flags it takes
    type CommandSignature =
        {
            Args: (string * Type) list
            OptArgs: (string * Type * Val) list
            Flags: Map<string, Type * Val>
        }

    type CommandExecutionContext =
        {
            Args: Val list
            Flags: Map<string, Val>
        }

    type Command =
        {
            Name: string
            Signature: CommandSignature
            Implementation: CommandExecutionContext -> Val
        }
        
    [<RequireQualifiedAccess>]
    type Err =
        | NotFound of string
        | Mismatch of string list
        | Unhandled of Exception

    type ReqResult = Result<Val, Err>