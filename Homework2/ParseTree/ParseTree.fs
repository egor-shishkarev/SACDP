namespace Trees

module ParseTree = 

    type Operation = 
        | Addition
        | Subtraction
        | Multiplication
        | Division

    type Tree =
        | Tree of Operation * Tree * Tree
        | Tip of int

    let rec evaluateTree tree =
        match tree with 
        | Tree (Addition, leftTree, rightTree) -> evaluateTree leftTree + evaluateTree rightTree
        | Tree (Subtraction, leftTree, rightTree) -> evaluateTree leftTree - evaluateTree rightTree
        | Tree (Multiplication, leftTree, rightTree) -> evaluateTree leftTree * evaluateTree rightTree
        | Tree (Division, leftTree, rightTree) -> 
            let resultOfRightTree = evaluateTree rightTree
            let resultOfLeftTree = evaluateTree leftTree
            if (resultOfRightTree = 0) then raise (System.DivideByZeroException("Деление на ноль!"))
            else 
                resultOfLeftTree / resultOfRightTree
        | Tip (number) -> number
