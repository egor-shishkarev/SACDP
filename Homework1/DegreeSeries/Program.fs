let degreeList n m =

    let rec fastPow acc pow = 
        if pow = 0 then 1
        else if pow % 2 = 0 then fastPow (acc * acc) (pow / 2)
        else acc * fastPow (acc * acc) ((pow - 1) / 2)

    let rec createList index list =
        if index = m then list
        else createList (index + 1) (List.head list * 2 :: list)

    let lst = createList 0 [fastPow 2 n]

    let rec reverseList newList index = 
        if index = List.length lst - 1 then newList
        else reverseList (lst[index + 1] :: newList) (index + 1)

    reverseList [] -1

let n = 2
let m = 8

printfn "List of degrees - %A" (degreeList n m)