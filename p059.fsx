let input = System.IO.File.ReadAllText "./p059_cipher.txt"

let rec keyGen n =
    match n with
    | 0 -> seq [[]]
    | _ -> {int('a') .. int('z')} |> Seq.collect (fun c -> keyGen (n - 1) |> Seq.map (fun k -> c::k))

let cipher = input.Split([|','|]) |> Seq.map int

let seqRepeat s = Seq.initInfinite (fun _ -> s) |> Seq.concat

let xorDecrypt cipher key = 
    Seq.zip cipher (seqRepeat key) 
        |> Seq.map ((fun (c, k) -> c ^^^ k) >> char)
        |> System.String.Concat

keyGen 3
    |> Seq.map (xorDecrypt cipher)
    |> Seq.find (fun s -> s.IndexOf(" the ") >= 0)
    |> Seq.sumBy int