let conversion lst = 
    let rec subFunction newList index = 
        if index = List.length lst - 1 then newList
        else subFunction (lst[index + 1] :: newList) (index + 1)
    subFunction [] -1

let list = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1]
printfn "Initial list is - %A, reversed list - %A" list (conversion list)
