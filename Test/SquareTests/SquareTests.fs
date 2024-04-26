module SquareTests

open NUnit.Framework
open FsUnit
open PrintSquare

[<Test>]
let ``Square with the side less than zero can't be printed`` () = 
    (fun () -> getSquare 0 |> ignore) |> should throw typeof<System.ArgumentException>

[<Test>]
let ``Square with the side 1 should equal one star`` () = 
    let oneSideSquare = getSquare 1
    oneSideSquare |> should equal ["*"]


[<Test>]
let ``Square with the side 2 should equal 2 by 2 stars`` () = 
    let oneSideSquare = getSquare 2
    oneSideSquare |> should equal ["**"; "**"]

[<Test>]
let ``Square with the side 4 should equal as expected`` () = 
    let oneSideSquare = getSquare 4
    oneSideSquare |> should equal ["****"; "*  *"; "*  *"; "****"]
