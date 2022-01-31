open System

type binTree = 
    | Data of int
    | Child of Node
and Node = {data:int; leftChild:binTree; rightChild:binTree}

let rec createBinTree n i =
    if n<=1 then Data(i)
    else Child{data = i; leftChild = createBinTree (n-1) (i+1); rightChild = createBinTree (n-1) (i+1)}


let rec getLeafNodes root list =
    match root with
    | Data(s) -> list@[s]
    | Child(cNode) -> list@(getLeafNodes cNode.leftChild list)@(getLeafNodes cNode.rightChild list)
    
let depth = 3

let rec getNodesMatchLevel root list curLevel targetLevel =
    match root with
    | Data(s) -> 
        if curLevel = targetLevel then list@[s]
        else []
    | Child(cNode) -> 
        if curLevel = targetLevel then list@[cNode.data]
        else 
            list@(getNodesMatchLevel cNode.leftChild list (curLevel+1) targetLevel)@(getNodesMatchLevel cNode.rightChild list (curLevel+1) targetLevel)



printfn "Sum of the nodes of a binary tree when target level matched"
Console.WriteLine("Total sum of nodes of a binary tree when target level matched -> {0}", List.sum(getNodesMatchLevel (createBinTree depth 1) [] 1 2)) 
Console.WriteLine()
Console.WriteLine("----------------------------------------")