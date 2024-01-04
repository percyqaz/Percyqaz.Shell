namespace Percyqaz.Shell.Tests

open Percyqaz.Shell
open Percyqaz.Shell.Tree
open NUnit.Framework

[<TestFixture>]
type ``2: Bindings``() =

    let ctx = Context.Empty.WithVar("x", Val.Num 10)

    let expect theory =
        function
        | Ok res -> Assert.AreEqual(theory, res)
        | ParseFail err -> Assert.Fail(sprintf "Parse fail: %A" err)
        | RunFail err -> Assert.Fail(sprintf "Runtime fail: %A" err)

    let expect_runtime_err =
        function
        | Ok res -> Assert.Fail("Expected runtime fail")
        | ParseFail err -> Assert.Fail("Expected runtime fail")
        | RunFail err -> Assert.Pass()

    [<Test>]
    member this.Reference() =

        ctx.Evaluate("x * x") |> expect (Val.Num 100.0)
        ctx.Evaluate("{ let y = x + 5; y / x }") |> expect (Val.Num(1.5))

    [<Test>]
    member this.Reference_Not_Found() =

        ctx.Evaluate("y * y") |> expect_runtime_err
        ctx.Evaluate("{ { let z = 2; 3 }; z }") |> expect_runtime_err

    [<Test>]
    member this.Shadowing() =

        ctx.Evaluate("{ let x = x + 5; x / x }") |> expect (Val.Num(1.0))

    [<Test>]
    member this.Commands() =

        ctx.Evaluate("{ let f = |x,y| -> x + y; f 10 20 }") |> expect (Val.Num 30.0)
        ctx.Evaluate("{ let f = |x,y| -> x + y; f(10, 20) }") |> expect (Val.Num 30.0)

        ctx.Evaluate("{ let g = |x| -> |y| -> x + y; g(10)(20) }")
        |> expect (Val.Num 30.0)

        ctx.Evaluate("{ action: || -> \"Hello World!\" }.action()")
        |> expect (Val.Str "Hello World!")
