let fibonacci n = 
    let rec subFunction (firstNumber: uint64) (secondNumber: uint64) x =
        match x with 
        | 0 -> firstNumber
        | a -> subFunction (secondNumber) (firstNumber + secondNumber) (x - 1)
    subFunction 0UL 1UL n

let n = 100
printfn "%d Fibonacci number is - %d" n (fibonacci n)
