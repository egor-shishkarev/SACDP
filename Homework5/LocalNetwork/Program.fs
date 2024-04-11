﻿module LocalNetwork

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

    member this.GetConnectedComponents() =
        let visited = Array.create computers.Length false
        let mutable components = []

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

    member this.PerformInfect() =
        printfn("Список смежности сети - \n%A") connections
        printfn("Начальное состояние сети - ")
        this.PrintNetworkStatus()

        let components = this.GetConnectedComponents()
        let isInfectedComponent = List.map (fun (x: int list) -> 
            List.fold (fun acc i -> acc || computers.[i].Infected) false x) <| components
        let isAnythingToInfect = List.map (fun (x: int list) -> 
            List.fold (fun acc i -> acc || (not computers.[i].Infected && infectionProbability.[computers.[i].OS] > 0.0) && isInfectedComponent.[List.findIndex (fun i -> i = x) components]) false x)

        let isFinal = List.fold (fun acc x -> acc || x) false << isAnythingToInfect

        let mutable i = 1
        while (isFinal components) <> false do
            printfn("Шаг %d") i
            this.Step()
            this.PrintNetworkStatus()
            i <- i + 1

        printfn("Заражено максимально возможное количество компьютеров")

