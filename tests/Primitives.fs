namespace Percyqaz.Shell.Tests

open Percyqaz.Shell
open Percyqaz.Shell.Tree
open NUnit.Framework

[<TestFixture>]
type ``1: Evaluation``() =

    let ctx = Context.Empty

    let expect theory =
        function
        | Ok res -> Assert.AreEqual(theory, res)
        | ParseFail err -> Assert.Fail(sprintf "Parse fail: %A" err)
        | RunFail err -> Assert.Fail(sprintf "Runtime fail: %A" err)

    let expect_runtime_err =
        function
        | Ok res -> Assert.Fail("Expected runtime fail")
        | ParseFail err ->  Assert.Fail("Expected runtime fail")
        | RunFail err -> Assert.Pass()

    [<Test>]
    member this.Primitives () =

        ctx.Evaluate "3" |> expect (Val.Num 3.0)
        ctx.Evaluate "0.0002" |> expect (Val.Num 0.0002)

        ctx.Evaluate "True" |> expect (Val.Bool true)
        ctx.Evaluate "False" |> expect (Val.Bool false)
        ctx.Evaluate "Nil" |> expect (Val.Nil)

        ctx.Evaluate "\"Hello world\"" |> expect (Val.Str "Hello world")
        ctx.Evaluate "\"Nil\"" |> expect (Val.Str "Nil")
        ctx.Evaluate "\"\"" |> expect (Val.Str "")

    [<Test>]
    member this.Arrays () =

        ctx.Evaluate "[]" |> expect (Val.Arr [])
        ctx.Evaluate "[3]" |> expect (Val.Arr [Val.Num 3.0])
        ctx.Evaluate "[[], 0.0002]" |> expect (Val.Arr [Val.Arr []; Val.Num 0.0002])

    [<Test>]
    member this.Objects () =

        ctx.Evaluate "{}" |> expect (Val.Obj Map.empty)
        ctx.Evaluate "{ \"Hello\": \"World\" }" |> expect (Val.Obj (Map.ofList ["Hello", Val.Str "World"]))
        ctx.Evaluate "{ Hello: \"World\" }" |> expect (Val.Obj (Map.ofList ["Hello", Val.Str "World"]))
        ctx.Evaluate "{ Hello: \"Foo\", \"Hello\": \"World\" }" |> expect (Val.Obj (Map.ofList ["Hello", Val.Str "World"]))

    [<Test>]
    member this.Array_Indexing () =
        
        ctx.Evaluate "[3][0]" |> expect (Val.Num 3.0)
        ctx.Evaluate "[0.1][0.4]" |> expect (Val.Num 0.1)
        ctx.Evaluate "[True,2,Nil][1]" |> expect (Val.Num 2.0)

    [<Test>]
    member this.Array_Indexing_Errors () =
        
        ctx.Evaluate "[3][-1]" |> expect_runtime_err
        ctx.Evaluate "[][0]" |> expect_runtime_err
        ctx.Evaluate "[1,2,3][True]" |> expect_runtime_err
        ctx.Evaluate "[1,2,3][[]]" |> expect_runtime_err
        ctx.Evaluate "[0.1][0.9]" |> expect (Val.Num 0.1)

    // Object indexing
    // Object indexing errors

    [<Test>]
    member this.Arithmetic () =

        ctx.Evaluate "9 + 10" |> expect (Val.Num 19.0)
        ctx.Evaluate "10 - 9 - 8" |> expect (Val.Num -7.0)
        ctx.Evaluate "5 * 6 + 3 * 4" |> expect (Val.Num 42.0)
        ctx.Evaluate "5 * (6 + 3) * 4" |> expect (Val.Num 180.0)
        ctx.Evaluate "8 * (6 + 3) / 4" |> expect (Val.Num 18.0)
        
    [<Test>]
    member this.Logic () =

        ctx.Evaluate "True || False" |> expect (Val.Bool true)
        ctx.Evaluate "True || []" |> expect (Val.Bool true)
        ctx.Evaluate "True && False" |> expect (Val.Bool false)
        ctx.Evaluate "False && []" |> expect (Val.Bool false)
        ctx.Evaluate "!True" |> expect (Val.Bool false)
        ctx.Evaluate "!(False && False)" |> expect (Val.Bool true)

        // Falsy
        ctx.Evaluate "!?Nil" |> expect (Val.Bool true)
        ctx.Evaluate "!?0" |> expect (Val.Bool true)
        ctx.Evaluate "!?[]" |> expect (Val.Bool true)
        ctx.Evaluate "!?\"\"" |> expect (Val.Bool true)

        // Truthy
        ctx.Evaluate "!?\"Hello\"" |> expect (Val.Bool false)
        ctx.Evaluate "!?1" |> expect (Val.Bool false)
        ctx.Evaluate "!?True" |> expect (Val.Bool false)
        ctx.Evaluate "!?[1]" |> expect (Val.Bool false)