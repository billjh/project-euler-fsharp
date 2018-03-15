let input = """
75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
"""

// find the maximum number of rows of a triangular table for an array of int to fill
let maxRows (table:int[]) = Seq.initInfinite id |> Seq.find (fun r -> r * (r + 1) / 2 > table.Length) |> (fun x -> x - 1)

// get the cell value from a triangular table
let getCellValue (table:int[]) row col = table.[row * (row - 1) / 2 + col - 1]

let parseIntArray (s:string) = 
    s.Split([|' '; '\n'|]) 
    |> Seq.filter (System.Int32.TryParse >> fst) 
    |> Seq.map int
    |> Seq.toArray

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

let maxPathSum (table:int[]) = memoRec (fun maxPathSum (row, col) -> 
    match row = maxRows table with
    | true -> getCellValue table row col
    | false -> max (maxPathSum (row + 1, col)) (maxPathSum (row + 1, col + 1)) + getCellValue table row col)

maxPathSum (input |> parseIntArray) (1, 1)