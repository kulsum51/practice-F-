
let rec isPrimeNo n i =
    i > n/2 || (n % i <> 0 && isPrimeNo n (i + 1))


printfn "Check whether n is prime or not using recursion check till n/2"
let num2 = 37
let result2 = isPrimeNo num2 2
Console.WriteLine("{0} is prime? -> {1}", num2, result2) 
Console.WriteLine("Complexiry -> O(n)") 
Console.WriteLine()
Console.WriteLine("----------------------------------------")

