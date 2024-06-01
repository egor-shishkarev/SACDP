module SingleThreadTests

open NUnit.Framework
open FsUnit
open Lazy
open System

let getLazyFunctions = 
    let random = Random()
    let arrayOfFunctions = 
        [TestCaseData(SimpleLazy<obj>(fun () -> random.Next(100)));
        TestCaseData(ThreadSafeLazy<obj>(fun () -> random.Next(100))); 
        TestCaseData(LockFreeLazy<obj>(fun () -> random.Next(100)))]
    arrayOfFunctions

[<TestCaseSource("getLazyFunctions")>]
let ``Single thread mode should work as expected test`` (lazyObject: ILazy<obj>) =
    let firstGet = lazyObject.Get()
    let secondGet = lazyObject.Get()
    let thirdGet = lazyObject.Get()

    firstGet |> should equal secondGet
    secondGet |> should equal thirdGet
