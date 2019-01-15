// References:
// http://mathworld.wolfram.com/AnomalousCancellation.html

// from p005.fsx modified
let rec gcd x y = 
    match (x, y) with
    | (x, y) when x < y -> gcd y x
    | (x, y) when y = 0 -> x
    | (x, y) -> gcd y (x % y)

type Fraction (numerator : int, denominator : int) =
    member this.Numerator = numerator / (gcd numerator denominator)
    member this.Denominator = denominator / (gcd numerator denominator)
    static member (*) (f : Fraction, g : Fraction) = 
        Fraction(f.Numerator * g.Numerator, f.Denominator * g.Denominator)
    override this.GetHashCode() = hash (this.Numerator, this.Denominator)
    override this.Equals(b) =
        match b with
        | :? Fraction as f -> (this.Numerator, this.Denominator) = (f.Numerator, f.Denominator)
        | _ -> false

// from p024.fsx
let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

// from p024.fsx
let rec permutations = function
    | []      -> seq [[]]
    | x :: xs -> Seq.collect (insertions x) (permutations xs)

// facts about digit-cancelling fraction:
// 1. only two forms of fraction exist: ab/ca -> b/c and ba/ac -> b/c for all permutations (a, b, c)
// 2. none of the digits in fraction can be 0
permutations [1 .. 9]
    |> Seq.map(List.take 3 >> (fun p -> (p.[0], p.[1], p.[2])))
    |> Seq.distinct
    |> Seq.collect (fun (a, b, c) -> [ (Fraction(10*a+b, 10*c+a), Fraction(b, c)); (Fraction(10*b+a, 10*a+c), Fraction(b, c)) ])
    |> Seq.filter (fun (f, fs) -> f = fs)
    |> Seq.map (fun (f, _) -> f)
    |> Seq.filter (fun f -> f.Numerator < f.Denominator)
    |> Seq.reduce (*)
    |> (fun f -> f.Denominator)