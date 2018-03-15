let circle n =
    match n with
    | 1 -> 1
    | _ -> 16 * (pown n 2) - 28 * n + 16

{1 .. 501} |> Seq.sumBy circle