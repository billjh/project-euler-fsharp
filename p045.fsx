// from p044.fsx
let isPentagonal (n:int64) =
    let root = (1.0 + System.Math.Sqrt(1.0 + 24.0 * float(n))) / 6.0
    root |> int |> float = root

let isHexagonal (n:int64) = 
    let root = (1.0 + System.Math.Sqrt(1.0 + 8.0 * float(n))) / 4.0
    root |> int |> float = root

let triangleSeq = Seq.initInfinite id |> Seq.skip 1 |> Seq.map (int64 >> (fun n -> n * (n + 1L) / 2L))

triangleSeq
    |> Seq.skipWhile (fun n -> n <= 40755L)
    |> Seq.filter isPentagonal
    |> Seq.filter isHexagonal
    |> Seq.head