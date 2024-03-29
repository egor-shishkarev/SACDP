module Lambda

type Term = 
    | Var of string
    | App of Term * Term
    | Abs of string * Term

let rec containsFreeVar (expr: Term) (varName: string) =
    match expr with
    | Var v -> v = varName
    | Abs (v, body) -> v <> varName && containsFreeVar body varName
    | App (e1, e2) -> containsFreeVar e1 varName || containsFreeVar e2 varName

let rec alphaConversion (expr: Term) (oldVar: string) (newVar: string) =
    match expr with
    | Var v -> 
        if v = oldVar then
            Var newVar
        else 
            Var v
    | App (e1, e2) ->
        App (alphaConversion e1 oldVar newVar, alphaConversion e2 oldVar newVar)
    | Abs (v, body) ->
        if v = oldVar then
            Abs (newVar, alphaConversion body oldVar newVar)
        else 
            Abs (v, alphaConversion body oldVar newVar)

let rec findNewVar (expr: Term) (varName: string) (subExpr: Term) =
    let rec loop i =
        let newVar = sprintf "%s%d" varName i
        if not (containsFreeVar expr newVar) && not (containsFreeVar subExpr newVar) then
            newVar
        else
            loop (i + 1)
    loop 1

let rec substitute (expr: Term) (varName: string) (subExpr: Term) =
    match expr with
    | Var v -> 
        if v = varName then
            subExpr
        else 
            Var v
    | App (e1, e2) -> App (substitute e1 varName subExpr, substitute e2 varName subExpr)
    | Abs (v, body) ->
        if v = varName then
            Abs (v, body)
        else if not (containsFreeVar body v) then
            Abs (v, substitute body varName subExpr)
        else
            let newVar = findNewVar body varName subExpr
            let newBody = alphaConversion body v newVar
            Abs (newVar, substitute newBody varName subExpr)

let rec betaConversion (expr: Term) =
    match expr with
    | App ( Abs (v, body), arg) -> substitute body v arg
    | App (e1, e2) -> App (betaConversion e1, betaConversion e2)
    | Abs (v, body) -> Abs (v, betaConversion body)
    | _ -> expr

let rec normalize (expr: Term) =
    let reducedTerm = betaConversion expr
    if expr <> reducedTerm then
        normalize reducedTerm
    else
        reducedTerm

let rec toString (expr: Term) =
    match expr with
    | Var v -> v
    | App (e1, e2) -> $"({toString e1} {toString e2})"
    | Abs (v, body) -> $"λ{v}.{toString body}"

let expression = App ( Abs ("x", App (Var "x", Var "y")), Abs ("x", Var "x"))
printfn "%A" <| toString expression
printfn "%A" <| toString (normalize expression)
