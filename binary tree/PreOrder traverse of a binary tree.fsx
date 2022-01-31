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

let tree = createBinTree 4 1

let rec getPreOrder root list =
    match root with
    | Data(s) -> [s]
    | Child(cNode) -> list@[cNode.data]@(getPreOrder cNode.leftChild list)@(getPreOrder cNode.rightChild list)


printfn "PreOrder traverse of a binary tree"
printfn "PreOrder traverse: %A" (getPreOrder tree []) 
Console.WriteLine()
Console.WriteLine("----------------------------------------")

