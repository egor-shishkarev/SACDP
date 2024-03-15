module EvenNumbersTests 

open EvenNumbers.CountEvenNumbers
open NUnit.Framework
open FsUnit
open FsCheck

[<Test>]
let ``Count even numbers with map in list from 0 to 9 should return 5`` () =
    let list = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
    CountEvenNumbersWithMap list |> should equal 5

[<Test>]
let ``Count even numbers with map in empty list should return 0`` () =
    let list = []
    CountEvenNumbersWithMap list |> should equal 0

[<Test>]
let ``Count even numbers with map and with filter should be equal`` () =
    let areFunctionsIsEqual (list: list<int>) = CountEvenNumbersWithMap list = CountEvenNumbersWithFilter list
    Check.QuickThrowOnFailure areFunctionsIsEqual

[<Test>]
let ``Count even numbers with map and fold should be equal`` () =
    let areFunctionsIsEqual (list: list<int>) = CountEvenNumbersWithMap list = CountEvenNumbersWithFold list
    Check.QuickThrowOnFailure areFunctionsIsEqual