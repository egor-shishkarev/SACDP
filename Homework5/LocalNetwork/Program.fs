module LocalNetwork

open System

// Operating systems in local network
type OS = 
    | Windows
    | Linux
    | MacOS

// A local network unit - computer with operating system
type Computer (osType: OS) =
    let mutable infected = false
    
    member this.OS = osType
    member this.Infected = infected

    member this.Infect() =
        if not infected then
            infected <- true
        this

// Interface for mock-objects usage
type IRandom =
    abstract member NextDouble: unit -> float

// Default random function for cases without mock-objects
type DefaultRandom() =
        interface IRandom with
            override this.NextDouble() =
                Random().NextDouble()

// Implementation of local network
type LocalNetwork (computers: Computer list, connections: int list list, infectionProbability: Map<OS, float>, ?random: IRandom) =
    let random = defaultArg random (DefaultRandom() :> IRandom)

    member this.Computers = computers
    member this.Connections = connections
    member this.InfectionProbability = infectionProbability

    // One step of infection in network
    member this.Step() = 
        let mutable infectedOnThisStep = []
        let addInfected newInfected = infectedOnThisStep <- newInfected :: infectedOnThisStep

        for i = 0 to connections.Length - 1 do
            if computers.[i].Infected then
                for j in connections.[i] do
                    let connectedComputer = computers.[j]
                    if not connectedComputer.Infected then
                            let probability = infectionProbability.[connectedComputer.OS]
                            if probability > 0.0 && probability >= random.NextDouble() then
                                addInfected connectedComputer
        
        infectedOnThisStep <- List.map (fun (x: Computer) -> x.Infect()) infectedOnThisStep

    // Print current condition of local network
    member this.PrintNetworkStatus() =
        for i = 0 to computers.Length - 1 do
            printfn "%d - %A" i computers.[i].Infected

    // Returns connectivity components of local network
    member this.GetConnectedComponents() =
        let visited = Array.create computers.Length false
        let mutable components = []

        // Depth-first search
        let rec dfs node (comp: int list) =
            visited.[node] <- true
            let mutable comp = node :: comp
            List.iter (fun neighbour ->
                if not visited.[neighbour] then
                    comp <- dfs neighbour comp
            ) connections.[node]
            comp

        for i = 0 to connections.Length - 1 do
            if not visited.[i] then
                components <- dfs i [] :: components

        components

    // Infects computers while it is possible
    // Returns count of steps to infect all possible computers
    member this.PerformInfect() =
        printfn("Список смежности сети - \n%A") connections
        printfn("Начальное состояние сети - ")
        this.PrintNetworkStatus()

        let components = this.GetConnectedComponents()
        let isInfectedComponent = List.map (fun (x: int list) -> 
            List.fold (fun acc i -> acc || computers.[i].Infected) false x) <| components

        let containInfectedNeighbour(node: int) =
            connections[node]
            |> List.map (fun i -> computers[i].Infected)
            |> List.contains true

        let isAnythingToInfect = List.map (fun (x: int list) -> 
            List.fold (fun acc i -> acc || (not computers.[i].Infected && 
            (infectionProbability.[computers.[i].OS] > 0.0)
            && isInfectedComponent.[List.findIndex (fun i -> i = x) components] && containInfectedNeighbour(i))) false x)
            
        let isFinal = List.fold (fun acc x -> acc || x) false << isAnythingToInfect

        let mutable i = 1
        while isFinal components do
            printfn("Шаг %d") i
            this.Step()
            this.PrintNetworkStatus()
            i <- i + 1

        printfn("Заражено максимально возможное количество компьютеров")
        i - 1
