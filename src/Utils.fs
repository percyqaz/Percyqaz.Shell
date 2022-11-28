namespace Percyqaz.Shell

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
            (Runtime.Coerce.nil)
            (fun () -> Val.Nil)

    let bool : Type<bool> =
        create "Boolean"
            (Runtime.Coerce.bool)
            Val.Bool

    let int : Type<int> =
        create "Integer"
            (Runtime.Coerce.num >> int)
            (float >> Val.Num)

    let num : Type<float> =
        create "Number"
            (Runtime.Coerce.num)
            Val.Num

    let str : Type<string> =
        create "String"
            (Runtime.Coerce.string)
            Val.Str

    let list (ty: Type<'T>) : Type<'T list> =
        create ("List of " + ty.Name)
            (function Val.Arr xs -> List.map ty.Up xs | x -> unexpected x "Expected an array")
            (List.map ty.Down >> Val.Arr)

    let arr (ty: Type<'T>) : Type<'T array> =
        create ("Array of " + ty.Name)
            (function Val.Arr xs -> List.map ty.Up xs |> Array.ofList | x -> unexpected x "Expected an array")
            (Array.toList >> List.map ty.Down >> Val.Arr)

type private _Impl = Val list -> Val

type Impl =
    
    static member Create(f: unit -> 'T, ret: Type<'T>) : _Impl =
        fun xs ->
            if xs.Length <> 0 then failwith "Expected 0 arguments"
            else ret.Down (f())

    static member Create(f: unit -> unit) = Impl.Create(f, Types.nil)
    
    static member Create(
        a1: Type<'A>,
        f: 'A -> 'T,
        ret: Type<'T>
        ) : _Impl =
        fun xs ->
            match xs with
            | a :: [] -> f (a1.Up a) |> ret.Down
            | _ -> failwith "Expected 1 argument"

    static member Create(
        a1: Type<'A>,
        f: 'A -> unit
        ) = Impl.Create(a1, f, Types.nil)
    
    static member Create(
        a1: Type<'A>,
        a2: Type<'B>,
        f: 'A -> 'B -> 'T,
        ret: Type<'T>
        ) : _Impl =
        fun xs ->
            match xs with
            | a :: b :: [] -> f (a1.Up a) (a2.Up b) |> ret.Down
            | _ -> failwith "Expected 2 arguments"
            
    static member Create(
        a1: Type<'A>,
        a2: Type<'B>,
        f: 'A -> 'B -> unit
        ) = Impl.Create(a1, a2, f, Types.nil)
    
    static member Create(
        a1: Type<'A>,
        a2: Type<'B>,
        a3: Type<'C>,
        f: 'A -> 'B -> 'C -> 'T,
        ret: Type<'T>
        ) : _Impl =
        fun xs ->
            match xs with
            | a :: b :: c :: [] -> f (a1.Up a) (a2.Up b) (a3.Up c) |> ret.Down
            | _ -> failwith "Expected 3 arguments"
    
    static member Create(
        a1: Type<'A>,
        a2: Type<'B>,
        a3: Type<'C>,
        f: 'A -> 'B -> 'C -> unit
        ) = Impl.Create(a1, a2, a3, f, Types.nil)
    
    static member Create(
        a1: Type<'A>,
        a2: Type<'B>,
        a3: Type<'C>,
        a4: Type<'D>,
        f: 'A -> 'B -> 'C -> 'D -> 'T,
        ret: Type<'T>
        ) : _Impl =
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
        ) = Impl.Create(a1, a2, a3, a4, f, Types.nil)
        
    static member Create(
        a1: Type<'A>,
        a2: Type<'B>,
        a3: Type<'C>,
        a4: Type<'D>,
        a5: Type<'E>,
        f: 'A -> 'B -> 'C -> 'D -> 'E -> 'T,
        ret: Type<'T>
        ) : _Impl =
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
        ) : _Impl = Impl.Create(a1, a2, a3, a4, a5, f, Types.nil)

module Command =

    let create desc args impl : Func =
        {
            Desc = desc
            Binds = args
            Impl = impl
        }

    let create_anon (arity : int) (impl: _Impl) =
        create "Anonymous function"
            (seq {0 .. arity} |> List.ofSeq |> List.map (sprintf "arg%i"))
            impl
        |> Val.Func

module FuncTypes =

    open Types
    
    let func0 (ret: Type<'T>) : Type<unit -> 'T> =
        create (sprintf "%s -> %s" "Nil" ret.Name)
            (function Val.Func f -> (fun () -> ret.Up (f.Impl [])) | x -> unexpected x "Expected a function")
            (fun (f: unit -> 'T) -> Command.create_anon 0 (Impl.Create(f, ret)))

    let func1 (a1: Type<'A>, ret: Type<'T>) : Type<'A -> 'T> =
        create (sprintf "%s -> %s" a1.Name ret.Name)
            (function Val.Func f -> (fun a -> ret.Up (f.Impl [a1.Down a])) | x -> unexpected x "Expected a function")
            (fun (f: 'A -> 'T) -> Command.create_anon 0 (Impl.Create(a1, f, ret)))

    let func2 (a1: Type<'A>, a2: Type<'B>, ret: Type<'T>) : Type<'A -> 'B -> 'T> =
        create (sprintf "%s -> %s -> %s" a1.Name a2.Name ret.Name)
            (function Val.Func f -> (fun a b -> ret.Up (f.Impl [a1.Down a; a2.Down b])) | x -> unexpected x "Expected a function")
            (fun (f: 'A -> 'B -> 'T) -> Command.create_anon 0 (Impl.Create(a1, a2, f, ret)))

    let func3 (a1: Type<'A>, a2: Type<'B>, a3: Type<'C>, ret: Type<'T>) : Type<'A -> 'B -> 'C -> 'T> =
        create (sprintf "%s -> %s -> %s" a1.Name a2.Name ret.Name)
            (function Val.Func f -> (fun a b c -> ret.Up (f.Impl [a1.Down a; a2.Down b; a3.Down c])) | x -> unexpected x "Expected a function")
            (fun (f: 'A -> 'B -> 'C -> 'T) -> Command.create_anon 0 (Impl.Create(a1, a2, a3, f, ret)))

    let func4 (a1: Type<'A>, a2: Type<'B>, a3: Type<'C>, a4: Type<'D>, ret: Type<'T>) : Type<'A -> 'B -> 'C -> 'D -> 'T> =
        create (sprintf "%s -> %s -> %s" a1.Name a2.Name ret.Name)
            (function Val.Func f -> (fun a b c d -> ret.Up (f.Impl [a1.Down a; a2.Down b; a3.Down c; a4.Down d])) | x -> unexpected x "Expected a function")
            (fun (f: 'A -> 'B -> 'C -> 'D -> 'T) -> Command.create_anon 0 (Impl.Create(a1, a2, a3, a4, f, ret)))

    let func5 (a1: Type<'A>, a2: Type<'B>, a3: Type<'C>, a4: Type<'D>, a5: Type<'E>, ret: Type<'T>) : Type<'A -> 'B -> 'C -> 'D -> 'E -> 'T> =
        create (sprintf "%s -> %s -> %s" a1.Name a2.Name ret.Name)
            (function Val.Func f -> (fun a b c d e -> ret.Up (f.Impl [a1.Down a; a2.Down b; a3.Down c; a4.Down d; a5.Down e])) | x -> unexpected x "Expected a function")
            (fun (f: 'A -> 'B -> 'C -> 'D -> 'E -> 'T) -> Command.create_anon 0 (Impl.Create(a1, a2, a3, a4, a5, f, ret)))