let isPentagonal (n:int64) =
    let root = (1.0 + System.Math.Sqrt(1.0 + 24.0 * float(n))) / 6.0
    root |> int |> float = root

let pentagonalSeq = Seq.initInfinite id |> Seq.skip 1 |> Seq.map (int64 >> (fun n -> (3L*n*n - n)/2L))

pentagonalSeq
    |> Seq.collect (fun p -> pentagonalSeq |> Seq.takeWhile (fun q -> q < p) |> Seq.map (fun q -> (q, p)))
    |> Seq.find (fun (q, p) -> isPentagonal (p - q) && isPentagonal (p + q))
    |> (fun (q, p) -> p - q)