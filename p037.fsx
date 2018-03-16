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

// from p035.fsx modified
let length = string >> String.length

let rec truncateFromRight n =
    match n with
    | _ when n < 10L -> [n]
    | _ -> n::(truncateFromRight (n / 10L))

let rec truncateFromLeft n =
    match n with
    | _ when n < 10L -> [n]
    | _ -> n::(truncateFromLeft (n % (pown 10L (length n - 1))))

primes
    |> Seq.skipWhile (fun n -> n < 10L)
    |> Seq.filter (truncateFromLeft >> (Seq.forall isPrime))
    |> Seq.filter (truncateFromRight >> (Seq.forall isPrime))
    |> Seq.take 11
    |> Seq.sum