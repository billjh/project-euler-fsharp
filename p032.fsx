// from p024.fsx
let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

// from p024.fsx
let rec permutations = function
    | []      -> seq [[]]
    | x :: xs -> Seq.collect (insertions x) (permutations xs)

// For "multiplicand * multiplier = product" which total has 9 digits:
// -> Product must be 4 digits.
//     a. Considering 2 digits * 2 digits = 5 digits, since 99 * 99 < 10000, product is at most 4 digits
//     b. Considering 3 digits * 3 digits = 3 digits, since 100 * 100 > 999, product is at least 4 digits
// -> Multiplicand and multiplier (interchangable) must be either
//     a. 3 digits * 2 digits
//     b. 4 digits * 1 digits

let slicers = [(3, 2, 4); (4, 1, 4)]

let numerize = Array.rev >> Array.mapi(fun i d -> (pown 10 i) * d) >> Array.sum

let slice (s1, s2, s3) (digits:int[]) = (numerize digits.[0..s1-1], numerize digits.[s1..s1+s2-1], numerize digits.[s1+s2..s1+s2+s3-1])

permutations [1 .. 9]
    |> Seq.map List.toArray
    |> Seq.collect (fun p -> slicers |> Seq.map (fun s -> slice s p))
    |> Seq.filter (fun (a, b, c) -> a * b = c)
    |> Seq.map (fun (_, _, c) -> c)
    |> Seq.distinct
    |> Seq.sum