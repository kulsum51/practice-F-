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

let rec isMirrorLR root1 =
    match root1 with
    | Data(s) -> s.ToString()
    | Child(cNode) ->  (isMirrorLR cNode.leftChild).ToString() + cNode.data.ToString() + (isMirrorLR cNode.rightChild).ToString()

let rec isMirrorRL root1 =
    match root1 with
    | Data(s) -> s.ToString()
    | Child(cNode) ->  (isMirrorRL cNode.rightChild).ToString() + cNode.data.ToString() + (isMirrorRL cNode.leftChild).ToString()

let rec isSymmetric root =
    let str1 = isMirrorLR root
    let str2 = isMirrorRL root
    if str1=str2 then true
    else false


printfn "Symmetric binary tree"
printfn "Is Symmetric Tree: %b" (isSymmetric tree) 
Console.WriteLine()
Console.WriteLine("----------------------------------------")
