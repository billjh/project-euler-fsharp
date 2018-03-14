let sumOfSquare s = s |> Seq.sumBy (fun x -> x * x)
let squareOfSum s = s |> Seq.sum |> (fun x -> x * x)

{1 .. 100} |> (fun s -> squareOfSum s - sumOfSquare s)