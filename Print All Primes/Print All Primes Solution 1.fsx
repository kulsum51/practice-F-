open System

let rec printList (list:List<int>) =
    match list with
    | head :: tail -> sprintf "%d " head + printList(tail)
    | [] -> ""

let isPrime n x = n = x || x%n <> 0
        
let rec filterNonPrime listSqrtN listN = 
    match listSqrtN with
    | head :: tail -> filterNonPrime tail (List.filter (isPrime head) listN)
    | [] -> listN
    
let printAllPrime n limit =
    filterNonPrime [2 .. limit] [2 .. n]


printfn "Print all primes smaller than or equal to n checking till sqrt(n)"
let number = 50
let limitN = int(sqrt(float number))
let primeNumbers = printAllPrime number limitN
printfn "Prime numbers up to %d: %A" number primeNumbers
Console.WriteLine("Complexiry -> O(sqrt(n))") 
Console.WriteLine()
Console.WriteLine("----------------------------------------")    
