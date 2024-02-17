﻿let factorial n =
    let rec subFunction (acc: uint64) (x: uint64) = 
        if x = 0UL then acc
        else subFunction (acc * x) (x - 1UL)
    subFunction 1UL n

let n = 20UL
printfn "Factorial of %d is - %d" n (factorial n)
