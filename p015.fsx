// References:
// https://realworldocaml.org/v1/en/html/imperative-programming-1.html#memoization-and-dynamic-programming
// https://stackoverflow.com/a/19867052 (memoRec, lazy)
// https://stackoverflow.com/a/20568397 (memoize)

let memoize f =
    let cache = ref Map.empty
    fun x ->
        match (!cache).TryFind(x) with
        | Some res -> res
        | None ->
            let res = f x
            cache := (!cache).Add(x,res)
            res

let memoRec funcNorec = 
    let rec f = lazy (memoize (fun x -> funcNorec (f.Force()) x))
    f.Force()

let latticePath = memoRec (fun latticePath (n, m) -> 
    if n = 1L then m + 1L else {1L .. m} |> Seq.sumBy (fun m -> latticePath (n - 1L, m) |> (+) 1L))

latticePath (20L, 20L)