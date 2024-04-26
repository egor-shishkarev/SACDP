module Fibonacci

let rec fibonacci (firstNumber: int) (secondNumber: int) (sumOfNumbers: int) (index: int) =
    if firstNumber > 1_000_000 then
        sumOfNumbers
    else
        if (firstNumber % 2 = 0) then 
            fibonacci secondNumber (firstNumber + secondNumber) (sumOfNumbers + firstNumber) (index + 1)
        else 
            fibonacci secondNumber (firstNumber + secondNumber) (sumOfNumbers) (index + 1) 
       

printfn("%A") <| (fibonacci 1 1 0 0)
9
