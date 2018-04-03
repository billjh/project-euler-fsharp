let getDigits = string >> Seq.sort >> Seq.toArray

let allEqual s = Seq.forall (fun i -> i = Seq.head s) s

{1 .. System.Int32.MaxValue} |> Seq.find (fun n -> {1 .. 6} |> Seq.map ((fun i -> i * n) >> getDigits) |> allEqual)