// References:
// https://www.xarg.org/puzzle/project-euler/problem-26/
// https://theburningmonk.com/2010/09/project-euler-problem-26-solution/

let naturalNumber = Seq.initInfinite id |> Seq.skip 1

let rec cycleLength n =
    match n with
    | _ when n % 2I = 0I -> cycleLength (n / 2I)
    | _ when n % 5I = 0I -> cycleLength (n / 5I)
    | _ -> naturalNumber |> Seq.find (fun c -> ((pown 10I c) - 1I) % n = 0I)

{1I .. 999I} |> Seq.maxBy cycleLength