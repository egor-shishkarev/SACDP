let conversion lst = 
    let rec subFunction lst res = 
        match lst with 
        | [] -> res
        | hd :: tl -> subFunction (tl) (hd :: res)
    subFunction lst []

let list = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1]
printfn "Initial list is - %A, reversed list - %A" list (conversion list)
