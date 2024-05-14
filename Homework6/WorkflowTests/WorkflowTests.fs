module WorkflowTests

open NUnit.Framework
open FsUnit
open Workflow.Rounding
open Workflow.Calculate

[<Test>]
let ``Rounding workflow should work as expected`` () =
    let rounding = RoundingBuilder
    let result = rounding 3 {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return a / b
    }
    result |> should equal 0.048

[<Test>]
let ``Calculate workflow should work as expected`` () =
    let calculate = new CalculateBuilder()
    let result = calculate {
        let! x = "1"
        let! y = "2"
        return x + y
    }
    result.Value |> should equal 3

[<Test>]
let ``Calculate workflow with string that is not a number should return None`` () =
    let calculate = new CalculateBuilder()
    let result = calculate {
        let! x = "1"
        let! y = "Ú"
        let z = x + y
        return z
    }
    result |> should equal None
