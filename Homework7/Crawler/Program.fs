module Crawler

open System.Text.RegularExpressions
open System.Net.Http

let crawl url =
    let pattern = """<a href="http://(.*?)">"""
    let client = new HttpClient()

    let extractPattern pattern html =
        let regex = Regex(pattern)
        regex.Matches(html)
        |> Seq.cast<Match>
        |> Seq.map(fun m -> "http://" + m.Groups.[1].Value)
        |> Seq.toList

    let fetchAsync (url: string) =
        async {
            try
                let! response = client.GetStringAsync(url) |> Async.AwaitTask
                return Some (url, response)
            with
            | ex ->
                printfn "Failed to fetch %s: %s" url (ex.Message)
                return None
        }

    async {
        let! result = fetchAsync url
        match result with
        | Some (_, html) ->
            let links = extractPattern pattern html
            let fetchTasks = links |> List.map fetchAsync
            let! results = fetchTasks |> Async.Parallel
            let collectedResults =
                results
                |> Array.choose id
                |> Array.map (fun (url, content) -> (url, content.Length))
                |> Array.toList
            return Some(collectedResults)
        | None ->
            return None
    }
