// compare by log(base^power) = log(base) * power
System.IO.File.ReadAllLines "./p099_base_exp.txt"
    |> Seq.mapi (fun i (s:string) ->
        let ns = s.Split(',')
        let b = float ns.[0]
        let p = float ns.[1]
        i + 1, p * log b)
    |> Seq.maxBy snd
    |> fst
