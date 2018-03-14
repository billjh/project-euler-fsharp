let rec gcd (x:int64) (y:int64) = 
    match (x, y) with
    | (x, y) when x < y -> gcd y x
    | (x, y) when y = 0L -> x
    | (x, y) -> gcd y (x % y)

let lcm x y = x * y / (gcd x y)

{1L .. 20L} |> Seq.reduce lcm