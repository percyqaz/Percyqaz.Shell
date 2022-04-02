namespace Percyqaz.Shell.v2

module Types =

    open Tree
    
    [<AutoOpen>]
    module Helpers =
        
        let create<'T> (name: string) (up: Val -> 'T) (down: 'T -> Val) : Type<'T> =
            {
                Name = name
                Up = up
                Down = down
            }

        let unexpected x message = failwith ""

    let bool : Type<bool> =
        create "Boolean"
            (function Val.Bool b -> b | x -> unexpected x "Expected a boolean")
            Val.Bool

    let num : Type<float> =
        create "Number"
            (function Val.Num n -> n | x -> unexpected x "Expected a number")
            Val.Num

    let str : Type<string> =
        create "String"
            (function Val.Str s -> s | x -> unexpected x "Expected a string")
            Val.Str

    let list (ty: Type<'T>) : Type<'T list> =
        create ("List of " + ty.Name)
            (function Val.Arr xs -> List.map ty.Up xs | x -> unexpected x "Expected an array")
            (List.map ty.Down >> Val.Arr)

    let arr (ty: Type<'T>) : Type<'T array> =
        create ("Array of " + ty.Name)
            (function Val.Arr xs -> List.map ty.Up xs |> Array.ofList | x -> unexpected x "Expected an array")
            (Array.toList >> List.map ty.Down >> Val.Arr)