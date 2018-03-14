// [2; 3; 5; 7; 9; ...]
let nextFakePrime n = 
    match n with
    | 2 -> 3
    | _ -> n + 2

let rec factorCount n p =
    match n % p = 0 with
    | true -> 1 + factorCount (n / p) p
    | false -> 0

let rec power b p = 
    match p with
    | 0 -> 1
    | _ -> b * (power b (p - 1))

let rec divisorCount n factor =
    let currentCount = factorCount n factor
    match n with
    | 1 -> 1
    | _ -> (currentCount + 1) * divisorCount (n / (power factor currentCount)) (nextFakePrime factor)

let totalDivisorCount n = divisorCount n 2

// seq [1; 3; 6; 10; ...]
let triangularNumbers = Seq.initInfinite id |> Seq.skip 1 |> Seq.map (fun n -> (1 + n) * n / 2)

triangularNumbers |> Seq.find (fun n -> divisorCount n 2 > 500)