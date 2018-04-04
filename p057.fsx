// from p005.fsx modified
let rec gcd x y = 
    match (x, y) with
    | (x, y) when x < y -> gcd y x
    | (x, y) when y = 0I -> x
    | (x, y) -> gcd y (x % y)

let fractionNormalize (x, y) =
    let div = gcd x y
    (x / div, y / div)

let squareRootOfTwoExpSeq = Seq.unfold (fun (a, b) -> Some((a, b), fractionNormalize (a + 2I * b, a + b))) (3I, 2I)

let digitCount = string >> Seq.length

squareRootOfTwoExpSeq
    |> Seq.take 1000
    |> Seq.filter (fun (a, b) -> digitCount a > digitCount b)
    |> Seq.length