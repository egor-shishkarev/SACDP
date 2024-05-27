module Crawler

open System.Net
open System.IO
open System.Text.RegularExpressions

let crawl url =
    let pattern = """<a href="http://(.*?)">"""

    let extractPattern pattern html =
        let regex = Regex(pattern)
        regex.Matches(html)
        |> Seq.cast<Match>
        |> Seq.map(fun m -> "http://" + m.Groups.[1].Value)
        |> Seq.toList

    let fetchAsync (url: string) =
        async {
            try 
                let request = WebRequest.Create(url)
                use! response = request.AsyncGetResponse()
                use stream = response.GetResponseStream()
                use reader = new StreamReader(stream)
                let html = reader.ReadToEnd()
                return Some (url, html)
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
            
            results
            |> Array.choose id
            |> Array.iter (fun (url, content) -> printfn "%s - %d characters" url content.Length)
        | None ->
            printfn "Failed to fetch the url %s" url
    }

let startUrl = "http://example.com"

crawl startUrl |> Async.RunSynchronously
