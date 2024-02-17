let fibonacci n = 
    let rec subFunction (firstNumber: uint64) (secondNumber: uint64) x =
        if x = 0 then firstNumber
        else subFunction (secondNumber) (firstNumber + secondNumber) (x - 1)
    subFunction 0UL 1UL n

let n = 100
printfn "%d Fibonacci number is - %d" n (fibonacci n)
