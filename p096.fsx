type Cell =  Empty | Fixed of int | Guess of int

type Soduku = Cell [,]

let cellValue = function Empty -> None | Fixed n -> Some n | Guess n -> Some n

let isCompleteSolution (s:Soduku) = s |> Seq.cast<Cell> |> Seq.forall (function Empty -> false | _ -> true)

let checkSolution s =
    let getRows (soduku:Soduku) = Seq.map (fun c -> Seq.map (fun r -> soduku.[c,r]) {0 .. 8}) {0 .. 8}
    let getColumns (soduku:Soduku) = Seq.map (fun r -> Seq.map (fun c -> soduku.[c,r]) {0 .. 8}) {0 .. 8}
    let getBlocks (soduku:Soduku) = 
        [0;3;6] 
        |> Seq.collect (fun offset -> soduku |> getRows |> Seq.collect (Seq.skip offset >> Seq.take 3))
        |> Seq.chunkBySize 9
        |> Seq.map Seq.ofArray
    let noDuplicateCell =
        Seq.choose cellValue
        >> Seq.groupBy id 
        >> Seq.map (snd >> Seq.length) 
        >> Seq.forall (fun l -> l <= 1)
    let (@) = Seq.append
    (getRows s) @ (getColumns s) @ (getBlocks s) |> Seq.forall noDuplicateCell


let cellSeq (s:Soduku) = Seq.collect (fun c -> Seq.map (fun r -> c, r) {0 .. 8}) {0 .. 8} |> Seq.map (fun (c, r) -> c, r, s.[c, r])
let nextEmptyCell = cellSeq >> Seq.find     (fun (_, _, v) -> match v with Empty   -> true | _ -> false)
let lastGuessCell = cellSeq >> Seq.findBack (fun (_, _, v) -> match v with Guess _ -> true | _ -> false)

let rec solveSoduku (s:Soduku) =
    let rec next s =
        match lastGuessCell s with
        | c, r, Guess 9 ->
            s.[c,r] <- Empty
            next s
        | c, r, Guess n ->
            s.[c,r] <- Guess (n + 1)
            s
        | _ -> failwith "no solution"
    match checkSolution s with
    | true ->
        match isCompleteSolution s with
        | true -> s
        | false -> 
            let c, r, _ = nextEmptyCell s
            s.[c,r] <- (Guess 1)
            solveSoduku s
    | false ->
        s |> next |> solveSoduku

let parseCell = function 0 -> Empty | n -> Fixed n

let charToInt (c:char) = int c - int '0'

let parseSoduku = Array.tail >> Array.map (Seq.map charToInt >> Seq.toArray) >> Array.map (Array.map parseCell) >> array2D

System.IO.File.ReadAllLines "./p096_sudoku.txt"
    |> Array.chunkBySize 10
    |> Array.map parseSoduku
    |> Array.Parallel.map solveSoduku
    |> Array.sumBy (cellSeq >> Seq.map ((fun (_,_,c) -> c) >> cellValue) >> Seq.choose id >> Seq.take 3 >> Seq.toList >> (fun ns -> ns.[0] * 100 + ns.[1] * 10 + ns.[2]))