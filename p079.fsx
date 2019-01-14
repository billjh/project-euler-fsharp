// Reference: https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm

let rec getOrderedPairs = function
    | [] -> []
    | head::tail -> List.map (fun t -> head, t) tail @ getOrderedPairs tail

let insertGraphEdge vf vt (g: Map<'a, Set<'b>>) =
    match Map.tryFind vf g with
    | None    -> Map.add vf (Set.empty.Add vt) g
    | Some vs -> Map.add vf (Set.add vt vs) g

let nodesWithoutIncomingEdge g = 
    ((g |> Map.toList |> Seq.map fst |> Set.ofSeq) - (g |> Map.toList |> Seq.collect snd |> Set.ofSeq))
    |> Set.toList

let noIncomingEdge g n = g |> Map.toSeq |> Seq.collect (snd >> Set.toSeq) |> Seq.contains n |> not

// Kahn's algorithm:
// L ← Empty list that will contain the sorted elements
// S ← Set of all nodes with no incoming edge
// while S is non-empty do
//     remove a node n from S
//     add n to tail of L
//     for each node m with an edge e from n to m do
//         remove edge e from the graph
//         if m has no other incoming edges then
//             insert m into S
// if graph has edges then
//     return error   (graph has at least one cycle)
// else 
//     return L   (a topologically sorted order)

let topologicalSort graph =
    let rec visit s g =
        match s with
        | [] ->
            match Map.isEmpty g with
            | true -> []
            | false -> failwith "Not a directed acyclic graph (DAG)"
        | head::s' ->
            let g' = Map.remove head g
            head ::
            match Map.tryFind head g with
            | None    -> visit s' g'
            | Some ms -> visit (s' @ (ms |> Set.toList |> List.filter (noIncomingEdge g'))) g'
    visit (graph |> nodesWithoutIncomingEdge) graph

System.IO.File.ReadAllLines "./p079_keylog.txt"
    |> Seq.map (fun s -> s.ToCharArray() |> Array.map (fun c -> int(c) - int('0')))
    |> Seq.collect (List.ofArray >> getOrderedPairs)
    |> Seq.fold (fun g (k, v) -> insertGraphEdge k v g) Map.empty
    |> topologicalSort
