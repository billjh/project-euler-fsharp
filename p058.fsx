// from p003.fsx
let takeSqrt (n:int64) = n |> double |> System.Math.Sqrt |> int64

// from p003.fsx
let isPrime (n:int64) = 
    match n with
    | n when n < 2L -> false
    | n when n = 2L -> true
    | n when n % 2L = 0L -> false
    | n -> {3L .. 2L .. takeSqrt n} |> Seq.exists (fun (div:int64) -> n % div = 0L) |> not

let countPrimes = Array.filter isPrime >> Array.length

let spiralDiagonalSeq = Seq.unfold (fun (l, ds) -> Some((l, ds), (l + 1L, Array.map (fun offset -> Array.last ds + offset * 2L * l) [|1L .. 4L|]))) (1L, [|1L|])

spiralDiagonalSeq
    |> Seq.scan (fun (_, primes, total) ns -> (fst ns, primes + (ns |> snd |> countPrimes), total + (ns |> snd |> Array.length))) (0L, 0, 0)
    |> Seq.skip 2
    |> Seq.find (fun (_, primes, total) -> double primes / (double total) < 0.1)
    |> (fun (l, _, _) -> 2L * l - 1L)