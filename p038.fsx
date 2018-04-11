let isPandigital = 
    String.filter System.Char.IsDigit 
    >> Seq.toArray 
    >> Array.sort
    >> (=) [|'1' .. '9'|]

let findPandigitalProduct n =
    List.map (fun m -> n * m |> string) [1 .. 9]
    |> List.scan (+) ""
    |> List.filter isPandigital
    |> (fun l -> if l.IsEmpty then None else Some(l.Head))

{2 .. 9999}
    |> Seq.map findPandigitalProduct
    |> Seq.choose id
    |> Seq.maxBy int