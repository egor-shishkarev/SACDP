module BracketSequence

let checkBracketsBalance (inputString: string): bool = 
    let leftBrackets = ['('; '{'; '[']
    let rightBrackets = [')'; '}'; ']']

    let findPair bracket =
        match bracket with
        | symbol when List.contains symbol leftBrackets ->
            rightBrackets[List.findIndex (fun item -> item = symbol) leftBrackets]
        | symbol when List.contains symbol rightBrackets ->
            leftBrackets[List.findIndex (fun item -> item = symbol) rightBrackets]
        | _ -> char(0)

    let pop stack = 
        match stack with
        | h :: t -> t
        | [] -> []

    let top stack =
        match stack with
        | h :: t -> h
        | [] -> char(0)

    let push stack element =
        element :: stack

    let isEmpty stack =
        match stack with
        | [] -> true
        | h :: t -> false


    let rec innerFunction stack (inputString: string) index =
        if index >= inputString.Length then
            isEmpty stack
        else 
            match inputString[index] with
            | symbol when List.contains symbol leftBrackets -> 
                innerFunction (push stack symbol) inputString (index + 1)
            | symbol when List.contains symbol rightBrackets ->
                if findPair (top stack) = symbol then
                    innerFunction (pop stack) inputString (index + 1)
                else
                    false
            | _ -> innerFunction stack inputString (index + 1)

    innerFunction [] inputString 0
