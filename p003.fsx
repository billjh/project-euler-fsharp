let takeSqrt (n:int64) = n |> double |> System.Math.Sqrt |> int64

let isPrime (n:int64) = 
    match n with
    | n when n < 2L -> false
    | n when n = 2L -> true
    | n when n % 2L = 0L -> false
    | n -> {3L .. 2L .. takeSqrt n} |> Seq.exists (fun (div:int64) -> n % div = 0L) |> not

let num = 600851475143L

{3L .. 2L .. takeSqrt num}
    |> Seq.filter isPrime
    |> Seq.filter (fun p -> num % p = 0L)
    |> (fun s -> if Seq.isEmpty s then num else Seq.last s) 