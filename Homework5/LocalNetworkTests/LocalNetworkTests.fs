module LocalNetworkTests

open NUnit.Framework
open FsUnit
open LocalNetwork

[<Test>]
let ``Network with inevitable infection should work as depth first search``() =
    let computers = [Computer(OS.Windows).Infect(); Computer(OS.Linux);
        Computer(OS.Windows); Computer(OS.MacOS)]
    let connections = [[1; 2; 3]; [0]; [0]; [0]]
    let probability = Map[OS.Linux, 1.0; OS.Windows, 1.0; OS.MacOS, 1.0]
    let net = new LocalNetwork(computers, connections, probability)

    net.Step()
    net.Step()
    let areAllInfected = List.fold (fun acc (x: Computer) -> 
        acc && x.Infected) true net.Computers
    areAllInfected |> should equal true

[<Test>]
let ``Network with impossible infection shouldn't change`` () =
    let computers = [Computer(OS.Windows).Infect(); Computer(OS.Linux);
        Computer(OS.Windows); Computer(OS.MacOS)]
    let connections = [[1; 2; 3]; [0]; [0]; [0]]
    let probability = Map[OS.Linux, 0.0; OS.Windows, 0.0; OS.MacOS, 0.0]
    let net = new LocalNetwork(computers, connections, probability)

    let notInfectedComputersBeforeStep = List.filter (fun (x: Computer) -> not x.Infected) net.Computers
    net.Step()
    let notInfectedComputersAfterStep = List.filter (fun (x: Computer) -> not x.Infected) net.Computers
    notInfectedComputersAfterStep |> should equal notInfectedComputersBeforeStep

[<Test>]
let ``Network with multiple components should stop infecting when all possible computers are infected.`` () =
    let computers = [Computer(OS.Windows); Computer(OS.Linux).Infect();
        Computer(OS.Windows); Computer(OS.MacOS); Computer(OS.MacOS)]
    let connections = [[1; 2]; [0]; [0]; [4]; [3]]
    let probability = Map[OS.Linux, 1.0; OS.Windows, 1.0; OS.MacOS, 1.0]
    let net = new LocalNetwork(computers, connections, probability)
    net.PerformInfect()
    (net.Computers.[3].Infected || net.Computers.[4].Infected) |> should equal false

