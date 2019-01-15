let factorial n = { 2 .. n } |> Seq.fold (*) 1

let rec digitFacorialSum = function
    | 0 -> 0
    | n -> factorial (n % 10) + digitFacorialSum (n / 10)

let noneRepeatingCount n =
    let rec next n ns =
        match Set.contains n ns with
        | false -> next (digitFacorialSum n) (Set.add n ns)
        | true  -> ns |> Set.count
    next n Set.empty

[|1 .. 999999|]
    |> Array.Parallel.map noneRepeatingCount
    |> Seq.filter ((=) 60)
    |> Seq.length