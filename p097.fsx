let keepLastTenDigits n = n % 10000000000L

let mul a b = (a * b) |> keepLastTenDigits

let add a b = (a + b) |> keepLastTenDigits

{ 1 .. 7830457 } 
    |> Seq.map (fun _ -> 2L)
    |> Seq.fold mul 1L
    |> (mul) 28433L
    |> (add) 1L