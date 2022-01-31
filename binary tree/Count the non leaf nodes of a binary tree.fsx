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

let rec getNonLeafNodes root list =
    match root with
    | Data(s) -> []
    | Child(cNode) -> list@[cNode.data]@(getNonLeafNodes cNode.leftChild list)@(getNonLeafNodes cNode.rightChild list)
	
	
printfn "Count the non leaf nodes of a binary tree"
Console.WriteLine("Total non leaves -> {0}", List.length(getNonLeafNodes (createBinTree depth 1) [])) 
Console.WriteLine()
Console.WriteLine("----------------------------------------")