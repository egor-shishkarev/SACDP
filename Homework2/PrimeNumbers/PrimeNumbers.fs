namespace PrimeNumbers

open System

module PrimeNumbers =
    let getPrimeNumbers () = 
        let isPrime number = 
            if number = bigint 0 then false
            else 
                let limit = number |> float32 |> MathF.Sqrt |> MathF.Round |> bigint |> (+) (bigint 1)
                let rec cycle i = 
                    if i = limit + bigint 1 then true
                    else if number % i = bigint 0 && i <> bigint 1 && i <> number then false
                    else cycle (i + bigint 1)
                cycle (bigint 1)
        seq {
             yield! Seq.filter(fun number -> isPrime number) (Seq.initInfinite (fun number -> bigint number))
        }
