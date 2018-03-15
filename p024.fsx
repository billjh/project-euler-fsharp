// References:
// https://stackoverflow.com/a/2184129
// https://stackoverflow.com/a/3129136

let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let rec permutations = function
    | []      -> seq [[]]
    | x :: xs -> Seq.collect (insertions x) (permutations xs)

permutations [0L .. 9L]
    |> Seq.map (List.mapi (fun i d -> (pown 10L i) * d) >> List.sum)
    |> Seq.sort
    |> Seq.toList
    |> List.item 999999