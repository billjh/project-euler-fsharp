// from p004.fsx
let reverseString (s:string) = s.ToCharArray() |> Array.rev |> System.String

// from p004.fsx modified
let isPalindrome (x:System.Numerics.BigInteger) = string(x) = (string(x) |> reverseString)

let reverseDigits (x:System.Numerics.BigInteger) = x |> string |> reverseString |> System.Numerics.BigInteger.Parse

let isLychelNumberCandidate n = 
    Seq.unfold (fun state -> Some(state, state + reverseDigits state)) n
        |> Seq.skip 1
        |> Seq.take 50
        |> Seq.forall (isPalindrome >> not)

{1I .. 10000I} |> Seq.filter isLychelNumberCandidate |> Seq.length