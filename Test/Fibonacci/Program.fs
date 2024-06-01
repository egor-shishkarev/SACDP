module Fibonacci

let rec fibonacciSum (firstNumber: int) (secondNumber: int) (sumOfNumbers: int) (index: int) =
    if firstNumber > 1_000_000 then
        sumOfNumbers
    else
        match firstNumber with
        | a when a % 2 = 0 -> fibonacciSum secondNumber (firstNumber + secondNumber) (sumOfNumbers + firstNumber) (index + 1)
        | _ -> fibonacciSum secondNumber (firstNumber + secondNumber) (sumOfNumbers) (index + 1) 


