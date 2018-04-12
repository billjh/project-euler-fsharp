let keepLastTenDigits n = n % 10000000000L

let mul a b = (a * b) |> keepLastTenDigits

let add a b = (a + b) |> keepLastTenDigits

2L
    |> Seq.replicate 7830457
    |> Seq.fold mul 1L
    |> (mul) 28433L
    |> (add) 1L