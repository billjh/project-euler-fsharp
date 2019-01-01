// the number of rectangles in a [1, m] grid
//    1 + 2 + ... + m 
// => m * (m + 1) / 2
// => (M)

// the number of rectangles in a [n, m] grid 
//    1 * (M) + 2 * (M) + ... + n * (M)
// => n * (n + 1) / 2 * (M)
// => n * (n + 1) * m * (m + 1) / 4

let countRectangles n m =
    let c x = x * (x + 1) / 2
    (c n) * (c m)

type Grid = {n:int; m:int; count:int}
let makeGrid n m = {Grid.n = n; m = m; count = countRectangles n m}

let target = 2000000
Seq.initInfinite ((+) 1)
    |> Seq.map (fun n -> Seq.initInfinite ((+) n) |> Seq.map (makeGrid n) |> Seq.windowed 2 |> Seq.find (fun gs -> gs.[1].count >= target))
    |> Seq.takeWhile (fun gs -> gs.[0].count <= target)
    |> Seq.collect id
    |> Seq.sortBy (fun g -> abs (g.count - target))
    |> Seq.head
    |> (fun g -> g.n * g.m)