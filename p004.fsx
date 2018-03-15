let reverseString (s:string) = s.ToCharArray() |> Array.rev |> System.String

let isPalindrome (x:int) = string(x) = (string(x) |> reverseString)

{100 .. 999} 
    |> Seq.collect (fun n -> {n .. 999} |> Seq.map (fun m -> (n * m)))
    |> Seq.filter isPalindrome
    |> Seq.max