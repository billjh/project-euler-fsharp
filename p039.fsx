// from p009.fsx
// generate seq of tuple (a, b, c) where a <= b <= c and a + b + c = n
let abcSeq n = {1 .. n / 3} |> Seq.collect (fun a -> {a .. (n - a) / 2} |> Seq.map (fun b -> (a, b, n - a - b)))

// from p009.fsx
let isPythagoreanTriplet (a, b, c) = a * a + b * b = c * c

{3 .. 1000}
    |> Seq.maxBy (abcSeq >> Seq.filter (fun (a, b, c) -> a + b > c) >> Seq.filter isPythagoreanTriplet >> Seq.length)