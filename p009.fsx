// generate seq of tuple (a, b, c) where a <= b <= c and a + b + c = n
let abcSeq n = {1 .. n / 3} |> Seq.collect (fun a -> {a .. (n - a) / 2} |> Seq.map (fun b -> (a, b, 1000 - a - b)))

let isPythagoreanTriplet (a, b, c) = a * a + b * b = c * c

abcSeq 1000 
    |> Seq.find isPythagoreanTriplet 
    |> (fun (a, b, c) -> a * b * c)