namespace Trees

module MapForTrees = 
    type Tree<'a> =
        | Tree of 'a * Tree<'a> * Tree<'a>
        | Tip of 'a

    let rec mapForTrees (tree: Tree<'a>) func =
        match tree with 
            | Tree (node, leftTree, rightTree) -> Tree (func node, mapForTrees leftTree func, mapForTrees rightTree func)
            | Tip (node) -> Tip (func node)
