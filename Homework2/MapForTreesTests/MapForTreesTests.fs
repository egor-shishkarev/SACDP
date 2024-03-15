module MapForTreesTests

open NUnit.Framework
open FsUnit
open Trees.MapForTrees


[<Test>]
let ``Map for trees should work as expected`` () = 
    let tree = Tree (1, Tree (2, Tip 3, Tip 4), Tree (5, Tip 6, Tip 7))
    let testFunction = fun item -> item - 1
    let expectedTree = Tree (0, Tree (1, Tip 2, Tip 3), Tree (4, Tip 5, Tip 6))
    mapForTrees tree testFunction |> should equal expectedTree
