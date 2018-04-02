// from p003.fsx
let takeSqrt (n:int64) = n |> double |> System.Math.Sqrt |> int64

// from p003.fsx
let isPrime (n:int64) = 
    match n with
    | n when n < 2L -> false
    | n when n = 2L -> true
    | n when n % 2L = 0L -> false
    | n -> {3L .. 2L .. takeSqrt n} |> Seq.exists (fun (div:int64) -> n % div = 0L) |> not

// from p007.fsx
let primes = Seq.initInfinite id |> Seq.map int64 |> Seq.filter isPrime

let limit = 1000000L

let maxLength = primes |> Seq.scan (+) 0L |> Seq.skip 1 |> Seq.takeWhile (fun sum -> sum < limit) |> Seq.length

{1 .. maxLength}
    |> Seq.rev
    |> Seq.collect (fun l -> primes |> Seq.windowed l |> Seq.map Array.sum |> Seq.takeWhile (fun sum -> sum < limit))
    |> Seq.find isPrime