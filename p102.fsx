type Point = { X:int ; Y:int }

let origin = { X = 0 ; Y = 0}

// calculate double of triangle area
let doubleArea a b c = abs (a.X * (b.Y - c.Y) + b.X * (c.Y - a.Y) + c.X * (a.Y - b.Y))

let containOrigin a b c = doubleArea a b c = (doubleArea a b origin) + (doubleArea a c origin) + (doubleArea b c origin)

let parseThreePoints (r:string) = 
    match r.Split([|','|]) |> Array.map int with 
    | [| ax; ay; bx; by; cx; cy |] -> {X=ax;Y=ay}, {X=bx;Y=by}, {X=cx;Y=cy} 
    | _ -> failwith "parsing error"

System.IO.File.ReadAllLines "./p102_triangles.txt"
    |> Seq.map parseThreePoints
    |> Seq.filter (fun (a, b, c) -> containOrigin a b c)
    |> Seq.length
