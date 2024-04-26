module FibonacciTests

open NUnit.Framework
open FsUnit
open Fibonacci

[<Test>]
let ``Sum of all even fibonacci numbers less than 1 000 000 should be as expected`` () =
    let getSumOfNumbers = fibonacciSum 1 1 0 0 
    getSumOfNumbers |> should equal 1089154
