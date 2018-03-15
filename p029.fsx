let range = {2I .. 100I}

range 
    |> Seq.collect (fun a -> range |> Seq.map (fun b -> pown a (b |> string |> int)))
    |> Seq.distinct
    |> Seq.length