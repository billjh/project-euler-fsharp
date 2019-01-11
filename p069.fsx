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

let phi n =
    let factors = primeFactors n |> Set.toList
    n * (factors |> Seq.map (fun p -> p - 1L) |> Seq.reduce (*)) / (factors |> Seq.reduce (*))

[2L .. 1000000L]
    |> Seq.maxBy (fun n -> float n / float (phi n))