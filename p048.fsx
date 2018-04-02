{1 .. 1000}
    |> Seq.sumBy (fun n -> pown (bigint n) n)
    |> string 
    |> Seq.rev
    |> Seq.take 10
    |> Seq.rev
    |> System.String.Concat