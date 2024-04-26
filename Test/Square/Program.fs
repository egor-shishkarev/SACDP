module PrintSquare

let getSquare (n: int) =

    if n <= 0 then 
        invalidArg (nameof(n)) "The side of square can't be less than zero"

    let rec addSymbol totalCountOfStars currentIndex (symbol: string) (result: string) =
        if totalCountOfStars < 0 then
            ""
        else
            match currentIndex with
            | a when a = totalCountOfStars -> result
            | _ -> addSymbol totalCountOfStars (currentIndex + 1) symbol (result + symbol)

    let topOrBottom = addSymbol n 0 "*" ""
    let intermediates = (addSymbol (n - 2) 0 " " "*") + "*"

    let rec currentRow index (squareList: string list) = 
        match index with
        | a when a = n - 1 -> 
            topOrBottom :: squareList
        | 0 -> 
            currentRow (index + 1) (topOrBottom :: squareList)
        | _ -> 
            currentRow (index + 1) (intermediates :: squareList)

    currentRow 0 []

let rec print (squareList: string list) =
    match squareList with
    | h :: t ->
        printfn("%s") h
        print t
    | _ -> printfn("\n")

print (getSquare 1)


