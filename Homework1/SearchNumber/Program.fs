let searchNumber list x = 
    let rec subFunction list index = 
        match list with 
        | hd :: tl when hd = x -> index
        | [] -> -1
        | hd :: tl -> subFunction (tl) (index + 1)
    subFunction list 0

let list = [1; 2; 3; 5]
printfn "Index of 2 is %d, index of 4 is %d" (searchNumber list 2) (searchNumber list 4)
