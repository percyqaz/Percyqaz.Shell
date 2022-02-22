namespace Percyqaz.Shell

open System

module Library =

    open Tree
    open FParsec

    [<RequireQualifiedAccess>]
    type CmdErr =
    | ParseFailure of string
    | TypeFailure of Check.Res
    | RuntimeException of Exception

    type CommandResult = Result<Val, CmdErr>

    module Context =

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

            | Expr.Piped_Input -> ctx.Variables.[""]
            | Expr.Variable x -> if ctx.Variables.ContainsKey x then ctx.Variables.[x] else failwithf "Unrecognised variable: '%s'" x
            | Expr.Subscript (main, sub) -> 
                let m = eval_expr main ctx
                let s = eval_expr sub ctx
                match m with
                | Val.Array xs ->
                    match s with
                    | Val.Number f -> xs.[int f]
                    | _ -> failwith "Must subscript arrays with a number"
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
                | Result.Error err -> failwith "Error dispatching subcommand" // needs more info
            | Expr.Cond (cond, iftrue, iffalse) ->
                let c = eval_expr cond ctx
                if (match c with Val.Bool b -> b | _ -> failwith "Condition must be an expression") then // truthiness nyi
                    eval_expr iftrue ctx
                else eval_expr iffalse ctx
            | Expr.Try (ex, iferror) -> failwith "nyi needs some infrastructure"

        and resolve (req: CommandRequestEx) (ctx: Context) : CommandRequest =
            {
                Name = req.Name
                Args = req.Args |> List.map (fun ex -> eval_expr ex ctx)
                Flags = req.Flags |> Map.map (fun _ ex -> eval_expr ex ctx)
            }

        and dispatch (req: CommandRequest) (ctx: Context) : CommandResult =
            let cmd = ctx.Commands.[req.Name]
            try cmd.Implementation { Args = req.Args; Flags = req.Flags } |> CommandResult.Ok
            with err -> CommandResult.Error (CmdErr.RuntimeException err)

    type Context with
        member this.Execute(command: CommandRequestEx) : CommandResult =
            match Check.type_check_reqex Type.Any command this with
            | [] -> 
                let req = Context.resolve command this
                Context.dispatch req this
            | xs -> printfn ""; xs |> List.iter Check.Err.prettyPrint; CommandResult.Error (CmdErr.TypeFailure xs)
                
        member this.Execute(command: string) : CommandResult =
            match run Parser.commandParser command with
            | Success (res, _, _) -> this.Execute res
            | Failure (err, _, _) -> printfn "Parse failure: %s" err; CommandResult.Error (CmdErr.ParseFailure err)

        member this.Mainloop() =
            while true do 
                printf "> "
                this.Execute(Console.ReadLine())
                |> ignore