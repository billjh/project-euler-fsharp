let divisorSum n = {1 .. n - 1} |> Seq.filter (fun d -> n % d = 0) |> Seq.sum
let hasAmicablePair n = (n |> divisorSum |> divisorSum) = n && divisorSum n <> n

{1 .. 9999} |> Seq.filter hasAmicablePair |> Seq.sum