module PriorityQueue

type PriorityQueue() = 
    let mutable queue: ('a * int) list = []

    member this.Add(item: 'a, priority: int) = 
        if (List.exists(fun (item) -> snd item = priority) queue) then
            invalidOp "Element with this priority already exists in queue"
        queue <- List.sortBy(fun (a, b) -> b) ((item, priority) :: queue)

    member this.Get() =
        match queue with
        | h :: t -> 
            queue <- t
            fst h
        | _ -> invalidOp "The queue contains zero elements"

let queue = new PriorityQueue()
