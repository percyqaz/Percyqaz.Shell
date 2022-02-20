namespace Percyqaz.Shell

open System

module Library =

    open Tree
    open FParsec

    type Context =
        {
            Variables: Map<string, Val>
            Commands: Command list
            PipedVal: Val
        }

    module Context =

        open Check

        let rec eval_expr (ex: Expr) (ctx: Context) =
            match ex with
            
            | Expr.String s -> Val.String s
            | Expr.Number n -> Val.Number n
            | Expr.Bool b -> Val.Bool b
            | Expr.Null -> Val.Null
            | Expr.Object m -> m |> Map.map (fun _ ex -> eval_expr ex ctx) |> Val.Object
            | Expr.Array xs -> xs |> List.map (fun ex -> eval_expr ex ctx) |> Val.Array
            | Expr.Closure x ->
                match eval_expr x ctx with
                | Val.Closure req -> Val.Closure req
                | x -> failwithf "Expected a closure here but got %A" x

            | Expr.Piped_Input -> ctx.PipedVal
            | Expr.Variable x -> if ctx.Variables.ContainsKey x then ctx.Variables.[x] else failwithf "Unrecognised variable: '%s'" x
            | Expr.Subscript (main, sub) -> 
                let m = eval_expr main ctx
                let s = eval_expr sub ctx
                match m with
                | Val.Array xs ->
                    match s with
                    | Val.Number f -> xs.[int f]
                    | _ -> failwith "Must subscript arrays with a number"
                | Val.Object ms ->
                    match s with
                    | Val.String s -> if ms.ContainsKey s then ms.[s] else failwithf "Object has no such property '%s'" s
                    | _ -> failwith "Must subscript objects with a string"
                | _ -> failwith "This value is not subscriptable"
            | Expr.Property (main, prop) ->
                let m = eval_expr main ctx
                match m with
                | Val.Object ms ->
                    if ms.ContainsKey prop then ms.[prop] else failwithf "Object has no such property '%s'" prop
                | _ -> failwith "This value is not an object"
            | Expr.Evaluate_Command (rx: CommandRequestEx) ->
                let req = resolve rx ctx
                match dispatch req ctx with
                | Result.Ok v -> v
                | Result.Error err -> failwith "Error dispatching subcommand" // needs more
            | Expr.Cond (cond, iftrue, iffalse) ->
                let c = eval_expr cond ctx
                if (true) then //truthiness nyi
                    eval_expr iftrue ctx
                else eval_expr iffalse ctx
            | Expr.Try (ex, iferror) -> failwith "nyi needs some infrastructure"

        and resolve (req: CommandRequestEx) (ctx: Context) : CommandRequest =
            {
                Name = req.Name
                Args = req.Args |> List.map (fun ex -> eval_expr ex ctx)
                Flags = req.Flags |> Map.map (fun _ ex -> eval_expr ex ctx)
            }

        and dispatch (req: CommandRequest) (ctx: Context) : ReqResult =
            match List.tryFind (fun i -> i.Name = req.Name) ctx.Commands with
            | Some cmd ->
                match type_check_req cmd req with
                | Result.Ok() -> 
                    try cmd.Implementation { Args = req.Args; Flags = req.Flags } |> ReqResult.Ok
                    with err -> ReqResult.Error (Err.Unhandled err)
                | Result.Error errs -> ReqResult.Error (Err.Mismatch errs)
            | None -> ReqResult.Error (Err.NotFound (sprintf "Unrecognised command '%s'" req.Name))
            

    type Context with
        member this.Evaluate(command: CommandRequestEx) = Context.resolve command this
        member this.Evaluate(expr: Expr) = Context.eval_expr expr this

        member this.Mainloop() =
            while true do
                Console.ReadLine()
                |> run Parser.commandParser
                |> 
                    function
                    | Success (res, _, _) ->
                        try
                            let req = Context.resolve res this
                            match Context.dispatch req this with
                            | Result.Ok v -> printfn "OK: %O" v
                            | Result.Error e -> printfn "RUNFAIL: %A" e
                        with err -> printfn "RESFAIL: %A" err 
                    | Failure (err, _, _) -> printfn "PARSEFAIL: %A" err