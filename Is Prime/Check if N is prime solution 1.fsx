open System

let rec isPrimeRec (num:int) (i:int) : bool =
    if num <= 1 then false
    elif num = i then true
    elif num%i = 0 then false
    else 
        isPrimeRec num (i+1)
 
 
 
printfn "Check whether n is prime or not using recursion check till n"
let num = 4
let result = isPrimeRec num 2
Console.WriteLine("{0} is prime? -> {1}", num, result) 
Console.WriteLine("Complexiry -> O(n)") 
Console.WriteLine()
Console.WriteLine("----------------------------------------")

