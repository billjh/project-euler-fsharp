// from p005.fsx modified
let rec gcd x y = 
    match (x, y) with
    | (x, y) when x < y -> gcd y x
    | (x, y) when y = 0 -> x
    | (x, y) -> gcd y (x % y)

// from p071.fsx
type Fraction = { numerator:int ; denominator:int }

// from p071.fsx
let isReducedForm f = gcd f.numerator f.denominator = 1

{4 .. 12000}
    |> Seq.collect (fun d -> 
        { int (1.0/3.0 * float d |> ceil) .. int (1.0/2.0 * float d |> floor) }
        |> Seq.map (fun n -> { numerator = n ; denominator = d }))
    |> Seq.filter isReducedForm
    |> Seq.length