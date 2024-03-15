module PrimeNumbersTests

open NUnit.Framework
open PrimeNumbers.PrimeNumbers
open FsUnit

[<Test>]
let ``First several prime numbers should be the same as expected`` () =
    let firstTenPrimeNumbers = getPrimeNumbers () |> Seq.take 10 |> Seq.toList
    firstTenPrimeNumbers = ([1; 2; 3; 5; 7; 11; 13; 17; 19; 23] |> List.map bigint) |> should be True
