// References:
// https://stackoverflow.com/a/22551722

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

let length (n:int64) = n |> string |> String.length

let circular n = Seq.unfold (fun num -> Some(num, (pown 10L (length n - 1)) * (num % 10L) + num / 10L)) n |> Seq.take (length n)

primes
    |> Seq.takeWhile (fun n -> n < 1000000L)
    |> Seq.filter (circular >> Seq.forall isPrime)
    |> Seq.length