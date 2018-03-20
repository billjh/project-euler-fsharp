let champernowneConstantDigit n =
    Seq.initInfinite id
    |> Seq.skip 1
    |> Seq.map (fun d -> 9 * d * (pown 10 (d - 1)))
    |> Seq.scan (+) 1
    |> Seq.mapi (fun i startIndex -> (i + 1, startIndex))
    |> Seq.takeWhile (fun (_, startIndex) -> startIndex <= n)
    |> Seq.last
    |> (fun (d, startIndex) -> 
        let num = (n - startIndex) / d + 1 + (pown 10 (d - 1) - 1)
        string(num).[(n - startIndex) % d])
    |> (fun c -> int(c) - int('0'))

[1; 10; 100; 1000; 10000; 100000; 1000000]
    |> List.map champernowneConstantDigit
    |> List.reduce (*)