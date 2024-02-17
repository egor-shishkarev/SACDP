let searchNumber list x = 
    let rec subFunction index = 
        if index >= List.length list - 1 && list[index] <> x then - 1
        else if list[index] = x then index
        else subFunction (index + 1)
    subFunction 0

let list = [1; 2; 3; 5]
printfn "Index of 2 is %d, index of 4 is %d" (searchNumber list 2) (searchNumber list 4)