// from p004.fsx
let reverseString (s:string) = s.ToCharArray() |> Array.rev |> System.String

// from p004.fsx
let isPalindrome (x:int) = string(x) = (string(x) |> reverseString)

let isPalindromeBase2 (x:int) =
    let base2 = System.Convert.ToString(x, 2)
    base2 = reverseString base2

{1 .. 999999}
    |> Seq.filter isPalindrome
    |> Seq.filter isPalindromeBase2
    |> Seq.sum