let digitSum (n:System.Numerics.BigInteger) = n |> string |> Seq.sumBy (fun c -> int(c) - int('0'))

{1I .. 100I}
    |> Seq.collect (fun a -> {1 .. 100} |> Seq.map (pown a))
    |> Seq.map digitSum
    |> Seq.max