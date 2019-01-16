// from p003.fsx modified
let takeSqrt n = n |> double |> System.Math.Sqrt |> int

// from p003.fsx modified
let isPrime n = 
    match n with
    | n when n < 2 -> false
    | n when n = 2 -> true
    | n when n % 2 = 0 -> false
    | n -> {3 .. 2 .. takeSqrt n} |> Seq.exists (fun div -> n % div = 0) |> not

// from p007.fsx modified
let primes = Seq.initInfinite id |> Seq.filter isPrime |> Seq.cache

let countPrimeSummations total =
    let ps = primes |> Seq.takeWhile (fun p -> p < total) |> Seq.rev |> Seq.toList
    let rec count ps sum =
        match ps with
        | [] -> if sum = 0 then 1 else 0
        | head::tail -> (count tail sum) + if head > sum then 0 else (count ps (sum - head))
    count ps total

Seq.initInfinite id
    |> Seq.skip 10
    |> Seq.find (fun n -> countPrimeSummations n > 5000)