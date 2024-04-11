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
        this


type LocalNetwork (computers: Computer list, connections: int list list, infectionProbability: Map<OS, float>) =
    member this.Computers = computers
    member this.Connections = connections
    member this.InfectionProbability = infectionProbability
    member this.IsFinal = false

    member this.Step() = 
        let mutable infectedOnThisStep = []
        let addInfected newInfected = infectedOnThisStep <- newInfected :: infectedOnThisStep

        for i = 0 to connections.Length - 1 do
            if computers.[i].Infected then
                for j in connections.[i] do
                    let connectedComputer = computers.[j]
                    if not connectedComputer.Infected then
                            let probability = infectionProbability.[connectedComputer.OS]
                            if probability > 0.0 && probability >= Random().NextDouble() then
                                addInfected connectedComputer
        
        infectedOnThisStep <- List.map (fun (x: Computer) -> x.Infect()) infectedOnThisStep

    member this.PrintNetworkStatus() =
        for i = 0 to computers.Length - 1 do
            printfn "%d - %A" i computers.[i].Infected

let computers = [Computer(OS.Windows).Infect(); Computer(OS.Linux);
    Computer(OS.Windows); Computer(OS.Windows); Computer(OS.Linux);
    Computer(OS.MacOS)]
let connections = [[1]; [0; 5]; [5]; [5]; [5]; [1; 2; 3; 4]]

let net = new LocalNetwork(computers, connections,
    Map[OS.Linux, 1; OS.Windows, 1; OS.MacOS, 1])

net.Step()
net.PrintNetworkStatus()
Console.WriteLine()
net.Step()
net.PrintNetworkStatus()
Console.WriteLine()
net.Step()
net.PrintNetworkStatus()
 