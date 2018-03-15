// from p021.fsx
let divisorSum n = {1 .. n - 1} |> Seq.filter (fun d -> n % d = 0) |> Seq.sum

let isAbundant n = (divisorSum n) > n

let limit = 28123

let abundantNumbers = 
    Seq.initInfinite id 
        |> Seq.skip 1 
        |> Seq.takeWhile (fun n -> n <= limit) 
        |> Seq.filter isAbundant
        |> Seq.toList

let abundantSums = 
    abundantNumbers 
        |> Seq.collect (fun a -> abundantNumbers |> Seq.map (fun b -> a + b))
        |> Seq.filter (fun s -> s <= limit)
        |> Seq.distinct

({1 .. limit} |> Seq.sum) - (abundantSums |> Seq.sum)