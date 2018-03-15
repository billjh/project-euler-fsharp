let digitFifthPowerSum n = n |> string |> Seq.sumBy (string >> int >> (fun d -> pown d 5))

// since 9^5 * 6 = 354294 which means number larger than that
// cannot be written as the sum of fifth powers of their digits
{2 .. (pown 9 5) * 6}
    |> Seq.filter (fun n -> n = digitFifthPowerSum n)
    |> Seq.sum