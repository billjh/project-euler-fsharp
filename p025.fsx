// from p002.fsx modifed
let fibonacciSeq = Seq.unfold (fun (current, next) -> Some(current, (next, current + next))) (0I, 1I)

fibonacciSeq |> Seq.findIndex (string >> String.length >> (fun l -> l = 1000))