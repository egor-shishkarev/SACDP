module ParseTreeTests

open NUnit.Framework
open FsUnit
open Trees.ParseTree

[<Test>]
let ``Function should correct evaluate tree expression`` () = 
    let tree = Tree (Addition, Tree (Multiplication, Tip 2, Tip 3), Tree (Subtraction, Tip 3, Tip 1))
    evaluateTree tree |> should equal 8

[<Test>]
let ``Function should throw an error for division by zero`` () =
    let tree = Tree (Division, Tree (Multiplication, Tip 1, Tip 1), Tree(Subtraction, Tip 1, Tip 1))
    (fun() -> evaluateTree tree |> ignore) |> should throw typeof<System.DivideByZeroException>
