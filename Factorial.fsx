open System

Console.WriteLine("----------------------------------------")


let rec factorial n =
    if n <= 1 then 1
    else n * factorial(n-1)
    
	
printfn "Find factorial of n using recursion"
let x = 5
let res = factorial x
Console.WriteLine("Factorial of {0} is {1}", x, res)
Console.WriteLine("Complexiry -> O(n)")
Console.WriteLine()
Console.WriteLine("----------------------------------------")