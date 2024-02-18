let degreeList n m =

    let rec fastPow acc pow = 
        match pow with
        | 0 -> 1
        | a -> 
            match a % 2 with 
            | 0 ->  fastPow (acc * acc) (pow / 2)
            | a -> acc * fastPow (acc * acc) ((pow - 1) / 2)

    let rec createList index list =
        match index with 
        | 0 -> list 
        | a -> createList (index - 1) (List.head list * 2 :: list)

    let rec reverseList acc list = 
        match list with 
        | [] -> acc
        | hd :: tl -> reverseList (hd :: acc) (tl)  

    [fastPow 2 n] |> createList m |> reverseList []

let n = 2
let m = 8

printfn "List of degrees - %A" (degreeList n m)
