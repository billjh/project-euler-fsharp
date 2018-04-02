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

primes
    |> Seq.skipWhile (fun p -> p < 1000L)
    |> Seq.takeWhile (fun p -> p < 10000L)
    |> Seq.map (fun p -> [p; p + 3330L; p + 6660L])
    |> Seq.filter (List.forall isPrime)
    |> Seq.filter (fun ps -> ps.[0] |> string |> Seq.sort |> Seq.toArray = (ps.[1] |> string |> Seq.sort |> Seq.toArray))
    |> Seq.filter (fun ps -> ps.[0] |> string |> Seq.sort |> Seq.toArray = (ps.[2] |> string |> Seq.sort |> Seq.toArray))
