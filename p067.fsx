let input = System.IO.File.ReadAllText "./p067_triangle.txt"

// from p018.fsx
// find the maximum number of rows of a triangular table for an array of int to fill
let maxRows (table:int[]) = Seq.initInfinite id |> Seq.find (fun r -> r * (r + 1) / 2 > table.Length) |> (fun x -> x - 1)

// from p018.fsx
// get the cell value from a triangular table
let getCellValue (table:int[]) row col = table.[row * (row - 1) / 2 + col - 1]

// from p018.fsx
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

// from p018.fsx
let maxPathSum (table:int[]) = memoRec (fun maxPathSum (row, col) -> 
    match row = maxRows table with
    | true -> getCellValue table row col
    | false -> max (maxPathSum (row + 1, col)) (maxPathSum (row + 1, col + 1)) + getCellValue table row col)

maxPathSum (input |> parseIntArray) (1, 1)