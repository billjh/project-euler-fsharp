let quadratic (a, b) = (fun x -> x * x + a * x + b)

// from p003.fsx
let takeSqrt (n:int64) = n |> double |> System.Math.Sqrt |> int64

// from p003.fsx
let isPrime (n:int64) = 
    match n with
    | n when n < 2L -> false
    | n when n = 2L -> true
    | n when n % 2L = 0L -> false
    | n -> {3L .. 2L .. takeSqrt n} |> Seq.exists (fun (div:int64) -> n % div = 0L) |> not

let primeCount f = Seq.initInfinite id |> Seq.takeWhile (int64 >> f >> isPrime) |> Seq.length

let range = seq {-999L .. 999L}

range
    |> Seq.collect (fun a -> range |> Seq.map (fun b -> (a, b)))
    |> Seq.maxBy (quadratic >> primeCount)
    |> (fun (a, b) -> a * b)