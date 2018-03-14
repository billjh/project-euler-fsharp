// seq [1; 2; 3; 5; ...]
let fibonacciSeq = Seq.unfold (fun (current, next) -> Some(current, (next, current + next))) (1, 2)

fibonacciSeq |> Seq.filter (fun x -> x % 2 = 0) |> Seq.takeWhile (fun x -> x < 4000000) |> Seq.sum