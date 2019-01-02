let input = System.IO.File.ReadAllLines "./p081_matrix.txt"

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

let minPathSum (matrix:int[,]) =
    (Array2D.length1 matrix - 1, Array2D.length2 matrix - 1)
    |> memoRec (fun minPathSum (row, col) ->
        match (row, col) with
        | (0, 0) -> matrix.[0, 0]
        | (0, c) -> matrix.[0, c] + minPathSum (0, c - 1)
        | (r, 0) -> matrix.[r, 0] + minPathSum (r - 1, 0)
        | (r, c) -> matrix.[r, c] + min (minPathSum (r, c - 1)) (minPathSum (r - 1, c)))

input 
    |> Array.map (fun r -> r.Split([|','|]) |> Array.map int)
    |> array2D
    |> minPathSum