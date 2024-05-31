let factorial n =
    let rec subFunction (acc: uint64) (x: uint64) = 
        match x with
        | 1UL | 0UL -> acc
        | a -> subFunction (acc * x) (x - 1UL)
    subFunction 1UL n

let n = 20UL
printfn "Factorial of %d is - %d" n (factorial n)
