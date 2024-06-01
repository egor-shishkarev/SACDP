module LocalNetworkTests

open NUnit.Framework
open FsUnit
open LocalNetwork
open Foq

let boundaryCases =
    [TestCaseData(1.0, 1, Some [0; 1; 2; 3; 4]);
    TestCaseData(1.0, 0, None);
    TestCaseData(0.0, 1, Some [0; 1; 2; 3; 4]);
    TestCaseData(0.0, 0, None)]

[<Test>]
let ``Network with inevitable infection should work as depth first search test``() =
    let mockProbability = 0.5
    let randomMock = 
        Mock<IRandom>()
            .Setup(fun x -> <@x.NextDouble() @>)
            .Returns(mockProbability)
            .Create()
    let computers = [Computer(OS.Windows).Infect(); Computer(OS.Linux);
        Computer(OS.Windows); Computer(OS.MacOS)]
    let connections = [[1; 2; 3]; [0]; [0]; [0]]
    let probability = Map[OS.Linux, 0.9; OS.Windows, 0.8; OS.MacOS, 0.7]
    let net = new LocalNetwork(computers, connections, probability, randomMock)

    net.Step()
    let areAllInfected = List.fold (fun acc (x: Computer) -> 
        acc && x.Infected) true net.Computers
    areAllInfected |> should equal true

[<Test>]
let ``Network with impossible infection shouldn't change test`` () =
    let mockProbability = 0.3
    let randomMock = 
        Mock<IRandom>()
            .Setup(fun x -> <@x.NextDouble() @>)
            .Returns(mockProbability)
            .Create()
    let computers = [Computer(OS.Windows).Infect(); Computer(OS.Linux);
        Computer(OS.Windows); Computer(OS.MacOS)]
    let connections = [[1; 2; 3]; [0]; [0]; [0]]
    let probability = Map[OS.Linux, 0.0; OS.Windows, 0.0; OS.MacOS, 0.0]
    let net = new LocalNetwork(computers, connections, probability, randomMock)

    let notInfectedComputersBeforeStep = List.filter (fun (x: Computer) -> not x.Infected) net.Computers
    net.Step()
    let notInfectedComputersAfterStep = List.filter (fun (x: Computer) -> not x.Infected) net.Computers
    notInfectedComputersAfterStep |> should equal notInfectedComputersBeforeStep

[<Test>]
let ``Network with multiple components should stop infecting when all possible computers are infected test`` () =
    let mockProbability = 0.5
    let randomMock = 
        Mock<IRandom>()
            .Setup(fun x -> <@x.NextDouble() @>)
            .Returns(mockProbability)
            .Create()
    let computers = [Computer(OS.Windows); Computer(OS.Linux).Infect();
        Computer(OS.Windows); Computer(OS.MacOS); Computer(OS.MacOS)]
    let connections = [[1; 2]; [0]; [0]; [4]; [3]]
    let probability = Map[OS.Linux, 0.9; OS.Windows, 0.8; OS.MacOS, 0.6]
    let net = new LocalNetwork(computers, connections, probability, randomMock)

    let countOfSteps = net.PerformInfect()
    (net.Computers.[3].Infected || net.Computers.[4].Infected) |> should equal false
    countOfSteps |> should equal 2

[<Test>]
let ``Network in which one of the computers cannot be infected should stop the infection test`` () = 
    let mockProbability = 0.5
    let randomMock = 
        Mock<IRandom>()
            .Setup(fun x -> <@x.NextDouble() @>)
            .Returns(mockProbability)
            .Create()
    let computers = [Computer(OS.Windows).Infect(); Computer(OS.Windows); Computer(OS.Windows); 
    Computer(OS.MacOS); Computer(OS.Windows); Computer(OS.Windows);]
    let connections = [[1]; [0; 2; 3]; [1]; [1; 4; 5]; [3]; [3]];
    let probability = Map[OS.Linux, 0.9; OS.Windows, 0.8; OS.MacOS, 0.0]
    let net = new LocalNetwork(computers, connections, probability, randomMock)

    let countOfSteps = net.PerformInfect()
    (net.Computers.[3].Infected || net.Computers.[4].Infected || net.Computers.[5].Infected) |> should equal false
    countOfSteps |> should equal 2

[<Test>]
let ``Complicated test`` () = 
    let mockProbability = 0.7
    let randomMock = 
        Mock<IRandom>()
            .Setup(fun x -> <@x.NextDouble() @>)
            .Returns(mockProbability)
            .Create()
    let computers = [Computer(OS.Windows); Computer(OS.Windows); Computer(OS.Linux).Infect(); Computer(OS.Linux).Infect(); 
    Computer(OS.MacOS); Computer(OS.Linux); Computer(OS.MacOS); Computer(OS.Linux); Computer(OS.Windows); 
    Computer(OS.MacOS); Computer(OS.Windows); Computer(OS.Linux)]

    let connections = [[1]; [5; 0]; [6]; [5; 7]; [5]; [1; 3; 4]; [2; 10]; [9; 11]; [9]; [7; 8; 11]; [6]; [7; 9]]
    let probability = Map[OS.Linux, 0.9; OS.Windows, 0.8; OS.MacOS, 0.0]
    let net = new LocalNetwork(computers, connections, probability, randomMock)

    let countOfSteps = net.PerformInfect()
    (net.Computers.[9].Infected || net.Computers.[8].Infected || net.Computers.[6].Infected || net.Computers.[10].Infected) |> should equal false
    countOfSteps |> should equal 3

[<TestCaseSource("boundaryCases")>]
let ``Boundary cases test`` (mockProbability: float, infectionProbability, expectedInfected: int list option) =
    let randomMock =
        Mock<IRandom>()
            .Setup(fun x -> <@x.NextDouble() @>)
            .Returns(mockProbability)
            .Create()
    let computers = [Computer(OS.Windows); Computer(OS.Linux);
        Computer(OS.Windows); Computer(OS.MacOS); Computer(OS.MacOS).Infect()]
    let connections = [[1]; [4; 2; 0]; [1; 3]; [4; 2]; [1; 3]]
    let probability = Map[OS.Linux, infectionProbability; OS.Windows, infectionProbability; OS.MacOS, infectionProbability]
    let net = new LocalNetwork(computers, connections, probability, randomMock)

    net.PerformInfect() |> ignore
    match expectedInfected with
    | Some v -> List.fold (fun acc i -> acc && net.Computers.[i].Infected) true v |> should equal true
    | None ->
        let infectedCount = List.filter (fun (computer: Computer) -> computer.Infected) net.Computers |> List.length
        infectedCount |> should equal 1
