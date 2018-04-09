let squareDigitSum = string >> Seq.map (fun c -> int(c) - int('0')) >> Seq.sumBy (fun d -> d * d)

let rec endWith n =
    match n with
    | 1 -> 1
    | 89 -> 89
    | otherwise -> otherwise |> squareDigitSum |> endWith

{1 .. 9999999} |> Seq.filter (fun n -> n |> endWith = 89) |> Seq.length