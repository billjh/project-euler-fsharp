2I ** 1000 |> string |> Seq.sumBy (string >> System.Int32.Parse)