let input = System.IO.File.ReadAllText "./p022_names.txt"

input
    |> (fun s -> s.Split([|','|]))
    |> Seq.map (Seq.filter System.Char.IsLetter >> System.String.Concat)
    |> Seq.sort
    |> Seq.mapi (fun i n -> (i + 1) * Seq.sumBy (fun c -> int(c) - int('A') + 1) n)
    |> Seq.sum
