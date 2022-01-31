
let rec isPrimeNoSqrt n i limit =
    i > limit || (n % i <> 0 && isPrimeNoSqrt n (i + 1) limit)


printfn "Check whether n is prime or not using recursion check till sqrt(n)"
let num3 = 61
let limit = int(sqrt(float num2))
let result3 = isPrimeNoSqrt num3 2 limit
Console.WriteLine("{0} is prime? -> {1}", num3, result3) 
Console.WriteLine("Complexiry -> O(n)") 
Console.WriteLine()
Console.WriteLine("----------------------------------------")