module BracketSequenceTests

open NUnit.Framework
open FsUnit
open BracketSequence

[<Test>]
let ``Simple true test`` () =
    let sequence = "(){[]()}"
    checkBracketsBalance sequence |> should equal true


[<Test>]
let ``Simple false test`` () =
    let sequence = "}{"
    checkBracketsBalance sequence |> should equal false

[<Test>]
let ``Complicated true test`` () =
    let sequence = "Скобочная последовательность равна = ({[]()}[{[()]}]{[]}())"
    checkBracketsBalance sequence |> should equal true

[<Test>]
let ``Complicated false test`` () =
    let sequence = "( {[}] ) {[{()[{()}]}]}"
    checkBracketsBalance sequence |> should equal false