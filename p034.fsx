let rec factorial n =
    match n with
    | 0 -> 1
    | n -> n * factorial (n - 1)

// since 9! * 7 = 2540160, it would be the upper bound for such numbers
{10 .. 2540160}
    |> Seq.filter (fun n -> n |> string |> Seq.sumBy (string >> int >> factorial) = n)
    |> Seq.sum