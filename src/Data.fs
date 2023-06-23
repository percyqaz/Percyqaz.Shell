namespace Percyqaz.Shell

open System

module Data =

    // types and values. the basis of data shoveling
    type [<RequireQualifiedAccess>] Type =
        | Any
        | Number
        | Text
        | Nil

        | Atoms of (string * Type) list
        | Object of Map<string, Type>
        | Array of Type
        | Function of Type list * Type
        static member Boolean = Atoms ["True", Type.Nil; "False", Type.Nil]
        override this.ToString() =
            match this with
            | Any -> "Any"
            | Number -> "Number"
            | Text -> "Text"
            | Nil -> "Nil"
            
            | Atoms xs ->
                String.concat " | " (xs |> Seq.map (fun (label, ty) -> "'" + label + if ty <> Type.Nil then "(" + ty.ToString() + ")" else ""))
            | Object vals -> "{ " + String.concat ", " (vals |> Map.toSeq |> Seq.map (fun (key, ty) -> key + ": " + ty.ToString())) + " }"
            | Array ty -> "[" + ty.ToString() + "]"
            | Function (args, ret) -> 
                if args = [Type.Nil] then "_" else String.concat " -> " (args |> List.map string)
                + " -> " + ret.ToString()

    and [<CustomEquality; NoComparison>] Func =
        {
            Desc: string
            Signature: (string * Type) list * Type
            Impl: Val list -> Val
        }
        override this.Equals(other) = false
        override this.GetHashCode() = hash (this.Signature, this.Desc)
    
    and [<RequireQualifiedAccess; StructuralEquality; NoComparison>] Val =
        | Text of string
        | Num of float
        | Nil

        | Atom of string * Val
        | Obj of Map<string, Val>
        | Arr of Val list
        | Fun of Func
        override this.ToString() =
            match this with
            | Text s -> sprintf "%A" s
            | Num n -> sprintf "%O" n
            | Nil -> "Nil"

            | Atom (a, Nil) -> "'" + a
            | Atom (a, v) -> sprintf "'%s(%O)" a v
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
            | Fun f -> sprintf "<function of arity %i>" (fst f.Signature).Length