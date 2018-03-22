// from p003.fsx
let takeSqrt (n:int64) = n |> double |> System.Math.Sqrt |> int64

// from p003.fsx
let isPrime (n:int64) = 
    match n with
    | n when n < 2L -> false
    | n when n = 2L -> true
    | n when n % 2L = 0L -> false
    | n -> {3L .. 2L .. takeSqrt n} |> Seq.exists (fun (div:int64) -> n % div = 0L) |> not

// from p024.fsx
let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

// from p024.fsx
let rec permutations = function
    | []      -> seq [[]]
    | x :: xs -> Seq.collect (insertions x) (permutations xs)

Seq.collect (fun n -> permutations [1L .. n]) {2L .. 9L}
    |> Seq.map (List.map string >> String.concat "" >> int64)
    |> Seq.filter isPrime
    |> Seq.max