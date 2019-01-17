// Reference: https://theburningmonk.com/2010/10/project-euler-problem-78-solution/

// from p015.fsx
let memoize f =
    let cache = ref Map.empty
    fun x ->
        match (!cache).TryFind(x) with
        | Some res -> res
        | None ->
            let res = f x
            cache := (!cache).Add(x,res)
            res

// from p015.fsx
let memoRec funcNorec = 
    let rec f = lazy (memoize (fun x -> funcNorec (f.Force()) x))
    f.Force()

// pentagonal numbers n*(3n-1)/2, eg. 1, 2, 5, 7, 12, ... where n is 1, -1, 2, -2, 3, -3, ...
let pentagonalNumbers = Seq.unfold (fun n -> Some (n, n + 1)) 1 |> Seq.collect (fun n -> [ n ; -n ]) |> Seq.map (fun n -> n * (3 * n - 1) / 2)

// coefficient sequence, eg. +, +, -, -, +, +, -, -, ...
let coefficients = Seq.unfold (fun n -> Some (n, -n)) 1I |> Seq.collect (fun n -> [ n ; n ])

// partition function p(k) = p(k – 1) + p(k – 2) – p(k – 5) – p(k – 7) + p(k – 12) + p(k – 15) – p(k – 22) ...
let partition = 
    memoRec (fun partition -> function 
        | 0 | 1 -> 1I
        | k ->
        pentagonalNumbers
            |> Seq.map (fun n -> k - n)
            |> Seq.takeWhile (fun n -> n >= 0)
            |> Seq.map partition
            |> Seq.zip coefficients
            |> Seq.sumBy (fun (coe, pn) -> pn * coe))

Seq.unfold (fun n -> Some (n, n + 1)) 2 |> Seq.find (fun n -> partition n % 1000000I = 0I)