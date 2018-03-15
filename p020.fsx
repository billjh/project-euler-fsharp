let factorial n = {1I .. n} |> Seq.reduce (*)

factorial 100I |> string |> Seq.sumBy (fun c -> c.ToString() |> int)