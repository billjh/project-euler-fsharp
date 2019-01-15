// from p005.fsx modified
let rec gcd x y = 
    match (x, y) with
    | (x, y) when x < y -> gcd y x
    | (x, y) when y = 0 -> x
    | (x, y) -> gcd y (x % y)

type Fraction = { numerator:int ; denominator:int }

let isReducedForm f = gcd f.numerator f.denominator = 1

{8 .. 1000000}
    |> Seq.map (fun d -> { Fraction.numerator = (3.0/7.0 * float d) |> floor |> int ; denominator = d })
    |> Seq.filter isReducedForm
    |> Seq.maxBy (fun f -> (float f.numerator) / (float f.denominator))
    |> (fun f -> f.numerator)