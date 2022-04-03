namespace Percyqaz.Shell.v2

open Tree

type Type<'T> =
    {
        Name: string
        Up: Val -> 'T
        Down: 'T -> Val
    }

module Types =
    
    [<AutoOpen>]
    module Helpers =
        
        let create<'T> (name: string) (up: Val -> 'T) (down: 'T -> Val) : Type<'T> =
            {
                Name = name
                Up = up
                Down = down
            }

        let unexpected x message = failwithf "%O: %s" x message

    let any : Type<Val> =
        create "Any" id id

    let nil : Type<unit> =
        create "Unit" 
            (function Val.Nil -> () | x -> unexpected x "Expected nil")
            (fun () -> Val.Nil)

    let bool : Type<bool> =
        create "Boolean"
            (function Val.Bool b -> b | x -> unexpected x "Expected a boolean")
            Val.Bool

    let int : Type<int> =
        create "Integer"
            (function Val.Num n -> int n | x -> unexpected x "Expected an integer")
            (float >> Val.Num)

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


type private Impl = Val list -> Val

type Command =
    
    static member Create(f: unit -> 'T, ret: Type<'T>) : Impl =
        fun xs ->
            if xs.Length <> 0 then failwith "Expected 0 arguments"
            else ret.Down (f())

    static member Create(f: unit -> unit) = Command.Create(f, Types.nil)
    
    static member Create(
        a1: Type<'A>,
        f: 'A -> 'T,
        ret: Type<'T>
        ) : Impl =
        fun xs ->
            match xs with
            | a :: [] -> f (a1.Up a) |> ret.Down
            | _ -> failwith "Expected 1 argument"

    static member Create(
        a1: Type<'A>,
        f: 'A -> unit
        ) = Command.Create(a1, f, Types.nil)
    
    static member Create(
        a1: Type<'A>,
        a2: Type<'B>,
        f: 'A -> 'B -> 'T,
        ret: Type<'T>
        ) : Impl =
        fun xs ->
            match xs with
            | a :: b :: [] -> f (a1.Up a) (a2.Up b) |> ret.Down
            | _ -> failwith "Expected 2 arguments"
            
    static member Create(
        a1: Type<'A>,
        a2: Type<'B>,
        f: 'A -> 'B -> unit
        ) = Command.Create(a1, a2, f, Types.nil)
    
    static member Create(
        a1: Type<'A>,
        a2: Type<'B>,
        a3: Type<'C>,
        f: 'A -> 'B -> 'C -> 'T,
        ret: Type<'T>
        ) : Impl =
        fun xs ->
            match xs with
            | a :: b :: c :: [] -> f (a1.Up a) (a2.Up b) (a3.Up c) |> ret.Down
            | _ -> failwith "Expected 3 arguments"
    
    static member Create(
        a1: Type<'A>,
        a2: Type<'B>,
        a3: Type<'C>,
        f: 'A -> 'B -> 'C -> unit
        ) = Command.Create(a1, a2, a3, f, Types.nil)
    
    static member Create(
        a1: Type<'A>,
        a2: Type<'B>,
        a3: Type<'C>,
        a4: Type<'D>,
        f: 'A -> 'B -> 'C -> 'D -> 'T,
        ret: Type<'T>
        ) : Impl =
        fun xs ->
            match xs with
            | a :: b :: c :: d :: [] -> f (a1.Up a) (a2.Up b) (a3.Up c) (a4.Up d) |> ret.Down
            | _ -> failwith "Expected 4 arguments"
    
    static member Create(
        a1: Type<'A>,
        a2: Type<'B>,
        a3: Type<'C>,
        a4: Type<'D>,
        f: 'A -> 'B -> 'C -> 'D -> unit
        ) = Command.Create(a1, a2, a3, a4, f, Types.nil)
        
    static member Create(
        a1: Type<'A>,
        a2: Type<'B>,
        a3: Type<'C>,
        a4: Type<'D>,
        a5: Type<'E>,
        f: 'A -> 'B -> 'C -> 'D -> 'E -> 'T,
        ret: Type<'T>
        ) : Impl =
        fun xs ->
            match xs with
            | a :: b :: c :: d :: e :: [] -> f (a1.Up a) (a2.Up b) (a3.Up c) (a4.Up d) (a5.Up e) |> ret.Down
            | _ -> failwith "Expected 5 arguments"
        
    static member Create(
        a1: Type<'A>,
        a2: Type<'B>,
        a3: Type<'C>,
        a4: Type<'D>,
        a5: Type<'E>,
        f: 'A -> 'B -> 'C -> 'D -> 'E -> unit
        ) : Impl = Command.Create(a1, a2, a3, a4, a5, f, Types.nil)

module Command =

    let create desc args impl : Func =
        {
            Desc = desc
            Binds = args
            Impl = impl
        }