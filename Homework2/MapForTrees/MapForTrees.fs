namespace Trees

module MapForTrees = 
    type Tree<'a> =
        | Tree of 'a * Tree<'a> * Tree<'a>
        | Tip of 'a

    let rec mapForTrees func (tree: Tree<'a>)  =
        match tree with 
            | Tree (node, leftTree, rightTree) -> Tree (func node, mapForTrees func leftTree , mapForTrees func rightTree)
            | Tip (node) -> Tip (func node)
