
let rec RemoveAllMultiples (max:int) (index:int) (listN:List<int>) =
    let num = listN.Item(index)
    if num <= max then RemoveAllMultiples max (index+1) (listN |> List.filter (fun x -> x = num || x%num <> 0))
    else listN

let printAllPrimeUsingLambda n limit =
    RemoveAllMultiples limit 0 [2 .. n] 


printfn "Print all primes smaller than or equal to n checking till sqrt(n) - using lambda"
let input = 50
let limitInput = int(sqrt(float input))
let numbers = printAllPrimeUsingLambda input limitInput
Console.WriteLine("Prime numbers up to {0}: {1}", input, printList numbers) 
Console.WriteLine("Complexiry -> O(sqrt(n))") 
Console.WriteLine()
Console.WriteLine("----------------------------------------")

