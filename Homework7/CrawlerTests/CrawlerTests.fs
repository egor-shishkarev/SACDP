module CrawlerTests

open NUnit.Framework
open Crawler
open FsUnit

[<Test>]
let ``Crawler should work as expected test`` () =
    let startUrl = "http://176.109.104.145:3000"
    let content = crawl startUrl |> Async.RunSynchronously
    match content with
    | Some v -> v.Length |> should equal 4
    | None -> Assert.Fail()

[<Test>]
let ``Crawler should return expected links and lengths test``() =
    let startUrl = "http://176.109.104.145:3000"
    let content = crawl startUrl |> Async.RunSynchronously
    let expectedContent = [
        ("http://timetable.spbu.ru/", 9826);
        ("http://www.4stud.info/web-programming/protocol-http.html", 33261);
        ("http://www.kremlin.ru/", 42739); 
        ("http://www.sberbank.ru/ru/person", 789)]
    match content with
    | Some v -> v |> should equal expectedContent
    | None -> Assert.Fail()

[<Test>]
let ``Unexisted url should return none test``() =
    let startUrl = "http://unexisted"
    crawl startUrl |> Async.RunSynchronously |> should equal None
