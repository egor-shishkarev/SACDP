module PointFreeTests

open NUnit.Framework
open FsCheck
open PointFree

[<Test>]
let ``Check equality of functions`` () =
    let checkEquality x l = 
        let resultFunc = equalFunctions.func x l
        let resultFunc'1 = equalFunctions.func'1 x l
        let resultFunc'2 = equalFunctions.func'2 x l
        let resultFunc'3 = equalFunctions.func'3 x l

        resultFunc = resultFunc'1 && resultFunc'1 = resultFunc'2 && resultFunc'2 = resultFunc'3

    Check.QuickThrowOnFailure checkEquality
