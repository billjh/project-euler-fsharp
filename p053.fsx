let factorial n = 
    match n with
    | _ when n < 1I -> 1I
    | _ -> {1I .. n} |> Seq.reduce (*)

let comb n r = factorial n / factorial r / factorial (n - r)

{1I .. 100I} |> Seq.sumBy (fun n -> {1I .. n} |> Seq.filter (fun r -> comb n r > 1000000I) |> Seq.length)