// from p012.fsx modified
// [2; 3; 5; 7; 9; ...]
let nextFakePrime n = 
    match n with
    | 2L -> 3L
    | _ -> n + 2L

let primeFactors n = 
    let rec primeFactorsRec (n, c, ps:Set<System.Int64>) =
        match n % c = 0L with
        | true -> primeFactorsRec (n / c, c, ps.Add(c))
        | false -> 
            match n with
            | 1L -> ps
            | _ -> primeFactorsRec (n, nextFakePrime c, ps)
    primeFactorsRec (n, 2L, Set.empty)

{1L .. System.Int64.MaxValue}
    |> Seq.filter (fun n -> primeFactors n |> Set.count = 4)
    |> Seq.windowed 4
    |> Seq.find (fun ns -> Array.last ns - Array.head ns = 3L)
    |> Array.head