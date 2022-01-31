
let isPrimeNo n =
   let rec check i =
      i > n/2 || (n % i <> 0 && check (i + 1))
   check 2

let primesInN N = seq { for n in 2..N do if isPrimeNo n then yield n }


printfn "Print all primes checking till n/2 - using sequence"
let input2 = 50
let primeNumbers2 = primesInN input2

printf "Prime numbers up to %d: " input2 
for x in primeNumbers2 do
    printf "%d " x

Console.WriteLine()
Console.WriteLine("Complexiry -> O(n)") 
Console.WriteLine()
Console.WriteLine("----------------------------------------")


