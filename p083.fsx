[<CustomComparison;StructuralEquality>]
type Distance = 
    Infinity | Distance of int
    interface System.IComparable with
        member x.CompareTo yObj =
            match yObj with
            | :? Distance as y ->
                match x, y with
                | Infinity, Infinity -> 0
                | Infinity, Distance _ -> 1
                | Distance _, Infinity -> -1
                | Distance x', Distance y' -> compare x' y'
            | _ -> invalidArg "yObj" "Cannot compare with other type"

let add a b = 
    match a, b with 
    | Infinity, _ | _, Infinity -> Infinity
    | Distance a', Distance b' -> Distance (a' + b')

let distanceMatrix m =
    let d = Array2D.create (Array2D.length1 m) (Array2D.length2 m) Infinity
    d.[0,0] <- Distance m.[0,0]
    d

let unvisitedNodes m = Seq.collect (fun x -> Seq.map (fun y -> x, y) {0 .. Array2D.length2 m - 1}) {0 .. Array2D.length1 m - 1} |> Set.ofSeq

let isNeighbor a b =
    let ax, ay = a
    let bx, by = b
    match abs (ax - bx), abs (ay - by) with
    | 1, 0 | 0, 1 -> true
    | _           -> false

let get m (x, y) = Array2D.get m x y

let set m (x, y) v = Array2D.set m x y v

let minPathSum m =
    let rec djikstra m dist unvisited =
        match Set.isEmpty unvisited with
        | true -> dist |> Seq.cast<Distance> |> Seq.last
        | false ->
            let u = unvisited |> Seq.minBy (get dist)
            unvisited 
                |> Seq.filter (isNeighbor u)
                |> Seq.iter (fun v -> set dist v (min (get dist v) (add (get dist u) (get m v))))
            djikstra m dist (Set.remove u unvisited)

    djikstra (Array2D.map Distance m) (distanceMatrix m) (unvisitedNodes m)

System.IO.File.ReadAllLines "./p083_matrix.txt"
    |> Array.map (fun r -> r.Split([|','|]) |> Array.map int)
    |> array2D
    |> minPathSum