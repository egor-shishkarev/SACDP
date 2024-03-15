namespace EvenNumbers
open System

module CountEvenNumbers = 
    let CountEvenNumbersWithMap list = 
        list |> Seq.map (fun item -> if item % 2 = 0 then 1 else 0) |> Seq.sum

    let CountEvenNumbersWithFilter list =
        list |> Seq.filter (fun item -> item % 2 = 0) |> Seq.length

    let CountEvenNumbersWithFold list = 
        list |> Seq.fold (fun acc item -> (+) acc <| Math.Abs(item + 1) % 2) 0
