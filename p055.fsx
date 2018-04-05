// from p004.fsx
let reverseString (s:string) = s.ToCharArray() |> Array.rev |> System.String

// from p004.fsx modified
let isPalindrome (x:bigint) = string(x) = (string(x) |> reverseString)

let reverseDigits (x:bigint) = x |> string |> reverseString |> bigint.Parse

let isLychelNumberCandidate n = 
    Seq.unfold (fun state -> Some(state, state + reverseDigits state)) n
        |> Seq.skip 1
        |> Seq.take 50
        |> Seq.forall (isPalindrome >> not)

{1I .. 10000I} |> Seq.filter isLychelNumberCandidate |> Seq.length