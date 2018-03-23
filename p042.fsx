let input = System.IO.File.ReadAllText "./p042_words.txt"

let wordValue = Seq.sumBy (fun c -> int(c) - int('A') + 1)

let triangleNumberSeq = Seq.initInfinite id |> Seq.map (fun n -> (n + 1) * (n + 2) / 2)

let isTriangleNumber n =
    triangleNumberSeq
    |> Seq.takeWhile (fun t -> t <= n)
    |> Seq.contains n

input.Replace(@"""", "").Split([|','|])
    |> Array.toSeq
    |> Seq.map wordValue
    |> Seq.filter isTriangleNumber
    |> Seq.length