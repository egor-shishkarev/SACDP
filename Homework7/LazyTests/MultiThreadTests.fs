module MultThreadTests

open NUnit.Framework
open FsUnit
open Lazy
open System.Threading

let multiThreadTestFunciton (lazyObject: ILazy<obj>) (resetEvent: ManualResetEvent) =
    let tasks = Array.init 10 (fun _ -> async { return lazyObject.Get() })
    let resultsAsync = tasks |> Async.Parallel
    resetEvent.Set() |> ignore
    let tasksResult = resultsAsync |> Async.RunSynchronously
    let example = tasksResult[0]

    tasksResult
    |> Array.forall (fun object -> obj.ReferenceEquals(object, example))
    |> should be True

[<Test>]
let ``ThreadSafeLazy should work as expected test`` () =
    let resetEvent = new ManualResetEvent(false)
    let counter = ref 0
    let supplier () =
        resetEvent.WaitOne() |> ignore
        Interlocked.Increment counter |> ignore
        obj()
    let lazyObject: ILazy<obj> = ThreadSafeLazy<obj>(supplier)
    multiThreadTestFunciton lazyObject resetEvent 
    counter.Value |> should equal 1

[<Test>]
let ``LockFreeLazy should work as expected test`` () =
    let resetEvent = new ManualResetEvent(false)
    let counter = ref 0
    let supplier () =
        resetEvent.WaitOne() |> ignore
        Interlocked.Increment counter |> ignore
        obj()
    let lazyObject: ILazy<obj> = LockFreeLazy<obj>(supplier)
    multiThreadTestFunciton lazyObject resetEvent
