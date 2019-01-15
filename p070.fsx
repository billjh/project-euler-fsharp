// from p012.fsx modified
// [2; 3; 5; 7; 9; ...]
let nextFakePrime n = 
    match n with
    | 2L -> 3L
    | _ -> n + 2L

// from p047.fsx
let primeFactors n = 
    let rec primeFactorsRec (n, c, ps:Set<System.Int64>) =
        match n % c = 0L with
        | true -> primeFactorsRec (n / c, c, ps.Add(c))
        | false -> 
            match n with
            | 1L -> ps
            | _ -> primeFactorsRec (n, nextFakePrime c, ps)
    primeFactorsRec (n, 2L, Set.empty)

// from p069.fsx
let phi n =
    let factors = primeFactors n |> Set.toList
    n * (factors |> Seq.map (fun p -> p - 1L) |> Seq.reduce (*)) / (factors |> Seq.reduce (*))

let isPermutation a b = (a |> string |> Seq.sort |> Seq.toArray) = (b |> string |> Seq.sort |> Seq.toArray)

[|2L .. 9999999L|]
    |> Array.Parallel.map (fun n -> n, phi n)
    |> Seq.filter (fun (n, pn) -> isPermutation n pn)
    |> Seq.minBy (fun (n, pn) -> float n / float pn)
    |> fst

// Took 04:59:56.3645895 on my laptop