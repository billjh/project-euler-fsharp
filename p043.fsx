// from p024.fsx
let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

// from p024.fsx
let rec permutations = function
    | []      -> seq [[]]
    | x :: xs -> Seq.collect (insertions x) (permutations xs)

let formNumber digits = 
    let length = List.length digits
    List.mapi (fun i d -> d * (pown 10L (length - 1 - i))) digits |> List.sum

permutations [0L .. 9L]
    |> Seq.filter (fun l -> l.Head <> 0L)
    |> Seq.filter (fun l -> formNumber l.[1 .. 3] % 2L = 0L)
    |> Seq.filter (fun l -> formNumber l.[2 .. 4] % 3L = 0L)
    |> Seq.filter (fun l -> formNumber l.[3 .. 5] % 5L = 0L)
    |> Seq.filter (fun l -> formNumber l.[4 .. 6] % 7L = 0L)
    |> Seq.filter (fun l -> formNumber l.[5 .. 7] % 11L = 0L)
    |> Seq.filter (fun l -> formNumber l.[6 .. 8] % 13L = 0L)
    |> Seq.filter (fun l -> formNumber l.[7 .. 9] % 17L = 0L)
    |> Seq.sumBy formNumber