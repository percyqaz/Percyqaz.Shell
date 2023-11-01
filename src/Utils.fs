namespace Percyqaz.Shell

open Percyqaz.Shell.Data

type TypeConversion<'T> =
    {
        Type: Type
        Up: Val -> 'T
        Down: 'T -> Val
    }

module TypeCoercion =

    let private error ex message = failwithf "%O : %s" ex message

    let rec string (value: Val) : string =
        match value with
        | Val.Text s -> s
        | Val.Atom(s, Val.Nil) -> s
        | Val.Atom(_, v) -> string v
        | _ -> value.ToString()

    let rec bool (value: Val) : bool =
        match value with
        | Val.Text s -> s <> ""
        | Val.Num n -> n <> 0
        | Val.Nil -> false
        | Val.Atom("True", _) -> true
        | Val.Atom("False", _) -> false
        | Val.Atom(_, v) -> bool v
        | Val.Arr xs -> not xs.IsEmpty
        | Val.Obj _ -> error value "Cannot cast object to bool"
        | Val.Fun _ -> error value "Cannot cast function to bool"

    open System
    open System.Globalization

    let rec num (value: Val) : float =
        match value with
        | Val.Text s ->
            let ok, r =
                Double.TryParse(
                    s,
                    NumberStyles.AllowLeadingSign ||| NumberStyles.AllowDecimalPoint,
                    CultureInfo.InvariantCulture
                )

            if ok then r else error value "Cannot cast value to num"
        | Val.Num n -> n
        | Val.Atom(_, n) -> num n
        | _ -> error value "Cannot cast value to num"

    let nil (_: Val) : unit = ()

module TypeConversion =

    [<AutoOpen>]
    module Helpers =

        let create<'T> (ty: Type) (up: Val -> 'T) (down: 'T -> Val) : TypeConversion<'T> =
            { Type = ty; Up = up; Down = down }

        let unexpected x message = failwithf "%O: %s" x message

    let any: TypeConversion<Val> = create Type.Any id id

    let nil: TypeConversion<unit> = create Type.Nil TypeCoercion.nil (fun () -> Val.Nil)

    let bool: TypeConversion<bool> =
        create Type.Boolean TypeCoercion.bool (fun b -> Val.Atom((if b then "True" else "False"), Val.Nil))

    let int: TypeConversion<int> =
        create Type.Number (TypeCoercion.num >> int) (float >> Val.Num)

    let num: TypeConversion<float> = create Type.Number TypeCoercion.num Val.Num

    let str: TypeConversion<string> = create Type.Text TypeCoercion.string Val.Text

    let list (ty: TypeConversion<'T>) : TypeConversion<'T list> =
        create
            (Type.Array ty.Type)
            (function
            | Val.Arr xs -> List.map ty.Up xs
            | x -> unexpected x "Expected an array")
            (List.map ty.Down >> Val.Arr)

    let arr (ty: TypeConversion<'T>) : TypeConversion<'T array> =
        create
            (Type.Array ty.Type)
            (function
            | Val.Arr xs -> List.map ty.Up xs |> Array.ofList
            | x -> unexpected x "Expected an array")
            (Array.toList >> List.map ty.Down >> Val.Arr)

    let auto_type<'T> () : TypeConversion<'T> =
        match typeof<'T> with
        | x when x = typeof<string> -> str |> unbox
        | x when x = typeof<int> -> int |> unbox
        | x when x = typeof<float> -> num |> unbox
        | x when x = typeof<bool> -> bool |> unbox
        | x when x = typeof<unit> -> nil |> unbox
        | x when x = typeof<Val> -> any |> unbox
        | x when x = typeof<string list> -> list str |> unbox
        | x when x = typeof<int list> -> list int |> unbox
        | x when x = typeof<float list> -> list num |> unbox
        | x when x = typeof<bool list> -> list bool |> unbox
        | x when x = typeof<Val list> -> list any |> unbox
        | x when x = typeof<string array> -> arr str |> unbox
        | x when x = typeof<int array> -> arr int |> unbox
        | x when x = typeof<float array> -> arr num |> unbox
        | x when x = typeof<bool array> -> arr bool |> unbox
        | x when x = typeof<Val array> -> arr any |> unbox
        | _t -> failwithf "Automatic type not supported for %O" _t

type private _Impl = (IOContext -> Val list -> Val) * Type list * Type

type Impl =

    // Arity 1
    static member Create1(a1: TypeConversion<'A>, f: 'A -> 'T, ret: TypeConversion<'T>) : _Impl =
        fun io xs ->
            match xs with
            | a :: [] -> f (a1.Up a) |> ret.Down
            | _ -> failwith "Expected 1 argument"
        , [ a1.Type ]
        , ret.Type

    static member Create1(f: 'A -> 'T) : _Impl =
        Impl.Create1(TypeConversion.auto_type<'A> (), f, TypeConversion.auto_type<'T> ())

    static member Create1(a1: TypeConversion<'A>, f: IOContext -> 'A -> 'T, ret: TypeConversion<'T>) : _Impl =
        fun io xs ->
            match xs with
            | a :: [] -> f io (a1.Up a) |> ret.Down
            | _ -> failwith "Expected 1 argument"
        , [ a1.Type ]
        , ret.Type

    static member Create1Io(f: IOContext -> 'A -> 'T) : _Impl =
        Impl.Create1(TypeConversion.auto_type<'A> (), f, TypeConversion.auto_type<'T> ())

    // Arity 2
    static member Create2
        (
            a1: TypeConversion<'A>,
            a2: TypeConversion<'B>,
            f: 'A -> 'B -> 'T,
            ret: TypeConversion<'T>
        ) : _Impl =
        fun io xs ->
            match xs with
            | a :: b :: [] -> f (a1.Up a) (a2.Up b) |> ret.Down
            | _ -> failwith "Expected 2 arguments"
        , [ a1.Type; a2.Type ]
        , ret.Type

    static member Create2(f: 'A -> 'B -> 'T) : _Impl =
        Impl.Create2(
            TypeConversion.auto_type<'A> (),
            TypeConversion.auto_type<'B> (),
            f,
            TypeConversion.auto_type<'T> ()
        )

    static member Create2
        (
            a1: TypeConversion<'A>,
            a2: TypeConversion<'B>,
            f: IOContext -> 'A -> 'B -> 'T,
            ret: TypeConversion<'T>
        ) : _Impl =
        fun io xs ->
            match xs with
            | a :: b :: [] -> f io (a1.Up a) (a2.Up b) |> ret.Down
            | _ -> failwith "Expected 2 arguments"
        , [ a1.Type; a2.Type ]
        , ret.Type

    static member Create2Io(f: IOContext -> 'A -> 'B -> 'T) : _Impl =
        Impl.Create2(
            TypeConversion.auto_type<'A> (),
            TypeConversion.auto_type<'B> (),
            f,
            TypeConversion.auto_type<'T> ()
        )

    // Arity 3
    static member Create3
        (
            a1: TypeConversion<'A>,
            a2: TypeConversion<'B>,
            a3: TypeConversion<'C>,
            f: 'A -> 'B -> 'C -> 'T,
            ret: TypeConversion<'T>
        ) : _Impl =
        fun io xs ->
            match xs with
            | a :: b :: c :: [] -> f (a1.Up a) (a2.Up b) (a3.Up c) |> ret.Down
            | _ -> failwith "Expected 3 arguments"
        , [ a1.Type; a2.Type; a3.Type ]
        , ret.Type

    static member Create3(f: 'A -> 'B -> 'C -> 'T) : _Impl =
        Impl.Create3(
            TypeConversion.auto_type<'A> (),
            TypeConversion.auto_type<'B> (),
            TypeConversion.auto_type<'C> (),
            f,
            TypeConversion.auto_type<'T> ()
        )

    static member Create3
        (
            a1: TypeConversion<'A>,
            a2: TypeConversion<'B>,
            a3: TypeConversion<'C>,
            f: IOContext -> 'A -> 'B -> 'C -> 'T,
            ret: TypeConversion<'T>
        ) : _Impl =
        fun io xs ->
            match xs with
            | a :: b :: c :: [] -> f io (a1.Up a) (a2.Up b) (a3.Up c) |> ret.Down
            | _ -> failwith "Expected 3 arguments"
        , [ a1.Type; a2.Type; a3.Type ]
        , ret.Type

    static member Create3Io(f: IOContext -> 'A -> 'B -> 'C -> 'T) : _Impl =
        Impl.Create3(
            TypeConversion.auto_type<'A> (),
            TypeConversion.auto_type<'B> (),
            TypeConversion.auto_type<'C> (),
            f,
            TypeConversion.auto_type<'T> ()
        )

    // Arity 4
    static member Create4
        (
            a1: TypeConversion<'A>,
            a2: TypeConversion<'B>,
            a3: TypeConversion<'C>,
            a4: TypeConversion<'D>,
            f: 'A -> 'B -> 'C -> 'D -> 'T,
            ret: TypeConversion<'T>
        ) : _Impl =
        fun io xs ->
            match xs with
            | a :: b :: c :: d :: [] -> f (a1.Up a) (a2.Up b) (a3.Up c) (a4.Up d) |> ret.Down
            | _ -> failwith "Expected 4 arguments"
        , [ a1.Type; a2.Type; a3.Type; a4.Type ]
        , ret.Type

    static member Create4(f: 'A -> 'B -> 'C -> 'D -> 'T) : _Impl =
        Impl.Create4(
            TypeConversion.auto_type<'A> (),
            TypeConversion.auto_type<'B> (),
            TypeConversion.auto_type<'C> (),
            TypeConversion.auto_type<'D> (),
            f,
            TypeConversion.auto_type<'T> ()
        )

    static member Create4
        (
            a1: TypeConversion<'A>,
            a2: TypeConversion<'B>,
            a3: TypeConversion<'C>,
            a4: TypeConversion<'D>,
            f: IOContext -> 'A -> 'B -> 'C -> 'D -> 'T,
            ret: TypeConversion<'T>
        ) : _Impl =
        fun io xs ->
            match xs with
            | a :: b :: c :: d :: [] -> f io (a1.Up a) (a2.Up b) (a3.Up c) (a4.Up d) |> ret.Down
            | _ -> failwith "Expected 4 arguments"
        , [ a1.Type; a2.Type; a3.Type; a4.Type ]
        , ret.Type

    static member Create4Io(f: IOContext -> 'A -> 'B -> 'C -> 'D -> 'T) : _Impl =
        Impl.Create4(
            TypeConversion.auto_type<'A> (),
            TypeConversion.auto_type<'B> (),
            TypeConversion.auto_type<'C> (),
            TypeConversion.auto_type<'D> (),
            f,
            TypeConversion.auto_type<'T> ()
        )

    // Arity 5
    static member Create5
        (
            a1: TypeConversion<'A>,
            a2: TypeConversion<'B>,
            a3: TypeConversion<'C>,
            a4: TypeConversion<'D>,
            a5: TypeConversion<'E>,
            f: 'A -> 'B -> 'C -> 'D -> 'E -> 'T,
            ret: TypeConversion<'T>
        ) : _Impl =
        fun io xs ->
            match xs with
            | a :: b :: c :: d :: e :: [] -> f (a1.Up a) (a2.Up b) (a3.Up c) (a4.Up d) (a5.Up e) |> ret.Down
            | _ -> failwith "Expected 5 arguments"
        , [ a1.Type; a2.Type; a3.Type; a4.Type; a5.Type ]
        , ret.Type

    static member Create5(f: 'A -> 'B -> 'C -> 'D -> 'E -> 'T) : _Impl =
        Impl.Create5(
            TypeConversion.auto_type<'A> (),
            TypeConversion.auto_type<'B> (),
            TypeConversion.auto_type<'C> (),
            TypeConversion.auto_type<'D> (),
            TypeConversion.auto_type<'E> (),
            f,
            TypeConversion.auto_type<'T> ()
        )

    static member Create5
        (
            a1: TypeConversion<'A>,
            a2: TypeConversion<'B>,
            a3: TypeConversion<'C>,
            a4: TypeConversion<'D>,
            a5: TypeConversion<'E>,
            f: IOContext -> 'A -> 'B -> 'C -> 'D -> 'E -> 'T,
            ret: TypeConversion<'T>
        ) : _Impl =
        fun io xs ->
            match xs with
            | a :: b :: c :: d :: e :: [] -> f io (a1.Up a) (a2.Up b) (a3.Up c) (a4.Up d) (a5.Up e) |> ret.Down
            | _ -> failwith "Expected 5 arguments"
        , [ a1.Type; a2.Type; a3.Type; a4.Type; a5.Type ]
        , ret.Type

    static member Create5Io(f: IOContext -> 'A -> 'B -> 'C -> 'D -> 'E -> 'T) : _Impl =
        Impl.Create5(
            TypeConversion.auto_type<'A> (),
            TypeConversion.auto_type<'B> (),
            TypeConversion.auto_type<'C> (),
            TypeConversion.auto_type<'D> (),
            TypeConversion.auto_type<'E> (),
            f,
            TypeConversion.auto_type<'T> ()
        )

module Command =

    let create desc args (impl, argTypes, retType) : Func =
        {
            Desc = desc
            Signature = List.zip args argTypes, retType
            Impl = impl
        }
