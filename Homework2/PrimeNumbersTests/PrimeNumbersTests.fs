module PrimeNumbersTests

open NUnit.Framework
open PrimeNumbers.PrimeNumbers
open FsUnit


[<Test>]
let ``First several prime numbers should be the same as expected`` () =
    let firstTenPrimeNumbers = getPrimeNumbers () |> Seq.take 20 |> Seq.toList
    firstTenPrimeNumbers |> should equal [1, 2, 3, 5, 7, 11, 13, 17, 19, 23] |> bigint
