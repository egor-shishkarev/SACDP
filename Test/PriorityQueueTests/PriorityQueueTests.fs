module PriorityQueueTests

open NUnit.Framework
open FsUnit
open PriorityQueue

[<Test>]
let ``Trying to get an element from queue without elements should throw exception`` () =
   let queue = new PriorityQueue()
   (fun () -> queue.Get() |> ignore) |> should throw typeof<System.InvalidOperationException>

[<Test>]
let ``Priority queue should work as expected`` () =
    let queue = new PriorityQueue()
    queue.Add("second", 2)
    queue.Add("first", 1)
    queue.Add("third", 3)
    queue.Get() |> should equal "first"
    queue.Get() |> should equal "second"
    queue.Get() |> should equal "third"
