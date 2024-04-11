open System

type OS = 
    | Windows
    | Linux
    | MacOS

type Computer (osType: OS) =
    let mutable infected = false
    
    member this.OS = osType
    member this.Infected = infected

    member this.Infect() =
        if not infected then
            infected <- true


type LocalNetwork (computers: Computer list, connections: bool list list, infectionProbability: Map<OS, float>) =
    member this.Computers = computers
    member this.Connections = connections
    member this.InfectionProbability = infectionProbability

    member this.Step() = 
        let mutable infectedOnThisStep = []
        let addInfected newInfected = infectedOnThisStep <- newInfected :: infectedOnThisStep

        for i = 0 to computers.Length - 1 do
            if computers.[i].Infected then
                for j = 0 to computers.Length - 1 do
                    if connections.[i].[j] && not computers.[j].Infected then
                        let probability = infectionProbability.[computers.[j].OS]
                        if probability > 0.0 && probability >= Random().NextDouble() then
                            addInfected computers.[j]
        List.iter (fun (x: Computer) -> x.Infect()) infectedOnThisStep

    member this.PrintNetworkStatus() =
        for i = 0 to computers.Length - 1 do
            printfn "%d - %A" i computers.[i].Infected

let computers = [Computer(OS.Windows); Computer(OS.Linux);
    Computer(OS.Windows); Computer(OS.Windows); Computer(OS.Linux);
    Computer(OS.MacOS)]
computers.[0].Infect()
let net = new LocalNetwork(computers, 
    [[false; true; false; false; false; false]; 
    [true; false; false; false; false; true]; 
    [false; false; false; false; false; true];
    [false; false; false; false; false; true]; 
    [false; false; false; false; false; true]; 
    [false; true; true; true; true; false]],
    Map[OS.Linux, 1; OS.Windows, 1; OS.MacOS, 1])

net.Step() |> ignore
net.PrintNetworkStatus()
Console.WriteLine()
net.Step() |> ignore
net.PrintNetworkStatus()
Console.WriteLine()
net.Step() |> ignore
net.PrintNetworkStatus()
 