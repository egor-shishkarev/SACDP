namespace Workflow
open System

module Rounding =
    type RoundingBuilder (numberOfDigitsAfterPoint: int) =
        member this.Bind (x: float, f) = 
            f (Math.Round(x, numberOfDigitsAfterPoint))
        member this.Return(x: float) =
            Math.Round(x, numberOfDigitsAfterPoint)

module Calculate =
    type CalculateBuilder() =
        member this.Bind (x: string, f) =
            match System.Double.TryParse(x) with
            | false, _ -> None
            | true, n -> f n
        member this.Return(x) = Some x
