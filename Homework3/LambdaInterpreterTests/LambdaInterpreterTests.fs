module LambdaInterpreterTests

open NUnit.Framework
open Lambda
open FsUnit

let K = Abs ("x", Abs ("y", Var "x"))
let I = Abs ("x", Var "x")
let S = Abs ("x", Abs ("y", Abs ("z", App ( App (Var "x", Var "z"), App (Var "y", Var "z")))))
let IF = Abs ("b", Abs ("t", Abs ("f", App ( App (Var "b", Var "t"), Var "f"))))
let TRUE = K
let FALSE = Abs ("x", Abs ("y", Var "y"))
let NOT = Abs ("b", App (IF, App ( App (Var "b", FALSE), TRUE)))

[<Test>]
let ``Test 1 - I I`` () =
    let expression = App (I, I)
    toString (normalize expression) |> should equal "λx.x"

[<Test>]
let ``Test 2 - K I`` () =
    let expression = App (K, I)
    toString (normalize expression) |> should equal "λy.λx.x"

[<Test>]
let ``Test 3 - S K K`` () =
    let expression = App ( App ( App(S, K), K), Var "someVar")
    toString (normalize expression) |> should equal "someVar"

[<Test>]
let ``Test 4 - IF TRUE`` () =
    let expression = App ( App ( App (IF, TRUE), Var "true"), Var "false")
    toString (normalize expression) |> should equal "true"

[<Test>]
let ``Test 5 - IF FALSE`` () =
    let expression = App ( App ( App (IF, FALSE), Var "true"), Var "false")
    toString (normalize expression) |> should equal "false"

[<Test>]
let ``Test 6 - NOT TRUE`` () =
    let expression = App ( App ( App (NOT, TRUE), Var "true"), Var "false")
    toString (normalize expression) |> should equal "false"

[<Test>]
let ``Test 7 - NOT FALSE`` () =
    let expression = App ( App (App (NOT, FALSE), Var "true"), Var "false")
    toString (normalize expression) |> should equal "true"