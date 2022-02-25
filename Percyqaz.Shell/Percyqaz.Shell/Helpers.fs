﻿namespace Percyqaz.Shell

open Tree

module Helpers =

    let echo : CommandInfo =
        {
            Signature =
                {
                    Args = []
                    OptArgs = []
                    Flags = Map.empty
                    ReturnType = Type.Null
                }
            Implementation = Unchecked.defaultof<_>
        }

    module Lens =
        
        let string = function Val.String s -> s :> obj | _ -> failwith "lens"
        let boolean = function Val.Bool b -> b :> obj | _ -> failwith "lens"
        let integer = function Val.Number i -> (int i) :> obj | _ -> failwith "lens"
        let float = function Val.Number n -> n :> obj | _ -> failwith "lens"
        let unit = function Val.Null -> () :> obj | _ -> failwith "lens"

        let create (ty: System.Type) =
            if ty = typeof<string> then string, Type.String
            elif ty = typeof<bool> then boolean, Type.Bool
            elif ty = typeof<int> then integer, Type.Number
            elif ty = typeof<float> then float, Type.Number
            elif ty = typeof<unit> then unit, Type.Null
            elif ty = typeof<Val> then (fun x -> x :> obj), Type.Any
            else (fun _ -> null), Type.Null

    module Return =

        let fromObj(o: obj) : Val =
            match o with
            | :? string as s -> Val.String s
            | :? bool as b -> Val.Bool b
            | :? int as i -> Val.Number (float i)
            | :? float as f -> Val.Number f
            | :? unit -> Val.Null
            | :? Val as v -> v
            | _ -> Val.Null

    let identifier (s: string) = s.ToLower()

    let createCommands<'T>() : (string * CommandInfo) seq =
        let methods = typeof<'T>.GetMethods()
        seq {
            for m in methods do
                if m.IsStatic then
                    let name = identifier m.Name

                    let ps = m.GetParameters()
                    let non_optional =
                        ps
                        |> Array.takeWhile (fun x -> not x.IsOptional)
                        |> Array.map (fun x -> identifier x.Name, Lens.create x.ParameterType)
                        |> List.ofArray
                    let optional = 
                        ps
                        |> Array.skipWhile (fun x -> not x.IsOptional)
                        |> Array.map (fun x -> identifier x.Name, Lens.create x.ParameterType)
                        |> List.ofArray

                    let args = non_optional |> List.map (fun (name, (_, ty)) -> (name, ty))
                    let optArgs = optional |> List.map (fun (name, (_, ty)) -> (name, ty))

                    let lenses = (non_optional @ optional) |> List.map (fun (_, (l, _)) -> l)

                    let impl = 
                        fun (ctx: CommandExecutionContext) ->
                            let args = Array.zeroCreate ps.Length
                            for i = 0 to ps.Length - 1 do
                                args.[i] <-
                                    if ctx.Args.Length > i then lenses.[i] ctx.Args.[i]
                                    else ps.[i].DefaultValue
                            m.Invoke(null, args) |> Return.fromObj

                    let _, returnTy = Lens.create m.ReturnType

                    yield 
                        name, {
                            Signature = 
                                {
                                    Args = args
                                    OptArgs = optArgs
                                    Flags = Map.empty
                                    ReturnType = returnTy
                                }
                            Implementation = impl
                        }
        }