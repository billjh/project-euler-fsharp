let rec collatzLength n =
    match n with
    | 1L -> 1
    | _ when n % 2L = 0L -> 1 + collatzLength (n / 2L)
    | _ -> 1 + collatzLength (3L * n + 1L)

{1L .. 1000000L} |> Seq.maxBy collatzLength