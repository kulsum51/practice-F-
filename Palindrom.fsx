open System


let rec isPalindrom (str:string) (startInd:int) (endInd:int) =
    startInd>endInd || (str.[startInd] = str.[endInd] && (isPalindrom str (startInd+1) (endInd-1)))

let checkPalindrom (str:String) =
    isPalindrom str 0 (str.Length-1)


printfn "Palindrom Check"
let s = "abcdcba" 
Console.WriteLine("'{0}' is palindrom? -> {1}", s, (checkPalindrom s)) 
Console.WriteLine()
Console.WriteLine("----------------------------------------")