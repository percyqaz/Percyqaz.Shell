namespace Percyqaz.Shell.Tests

open Percyqaz.Shell
open Percyqaz.Shell.Tree
open NUnit.Framework

[<TestFixture>]
type ``3: Shell Interface``() =

    let ctx = Context.Empty.WithVar("x", Val.Num 10)

    let expect_run: ShellResult<Context> -> Context =
        function
        | Ok ctx -> ctx
        | ParseFail err ->
            Assert.Fail(sprintf "Parse fail: %A" err)
            failwith ""
        | RunFail err ->
            Assert.Fail(sprintf "Runtime fail: %A" err)
            failwith ""

    let expect theory =
        function
        | Ok res -> Assert.AreEqual(theory, res)
        | ParseFail err -> Assert.Fail(sprintf "Parse fail: %A" err)
        | RunFail err -> Assert.Fail(sprintf "Runtime fail: %A" err)

    [<Test>]
    member this.Execute() =

        ctx
        |> fun ctx -> ctx.Interpret("let y = |x| -> x * x")
        |> expect_run
        |> fun ctx -> ctx.Interpret("let z = y 10")
        |> expect_run
        |> fun ctx -> ctx.Evaluate("z")
        |> expect (Val.Num 100.0)
