// from p057.fsx
let digitCount = string >> Seq.length >> bigint

let positiveNums = Seq.initInfinite id |> Seq.skip 1 |> Seq.map bigint

let powerSeq (n:bigint) = 
    positiveNums 
        |> Seq.map (fun b -> pown b (int n))
        |> Seq.skipWhile (fun p -> p |> digitCount < n)
        |> Seq.takeWhile (fun p -> p |> digitCount = n)

positiveNums
    |> Seq.map powerSeq
    |> Seq.takeWhile (fun s -> Seq.length s > 0)
    |> Seq.sumBy Seq.length