let hasForm format n =
    let nStr = string n
    match String.length format = String.length nStr with
    | false -> false
    | true -> Seq.zip format nStr |> Seq.forall (fun (f, d) -> f = '_' || f = d)

let lowerBound = 1020304050607080900.0 |> System.Math.Sqrt |> (fun n -> n / 10.0) |> System.Math.Floor |> int64 |> (*) 10L
let upperBound = 1929394959697989990.0 |> System.Math.Sqrt |> (fun n -> n / 10.0) |> System.Math.Ceiling |> int64 |> (*) 10L

{ lowerBound .. 10L .. upperBound }
    |> Seq.filter (fun n -> hasForm "1_2_3_4_5_6_7_8_9_0" (n * n))
    |> Seq.head