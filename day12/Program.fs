open Utils
let file = "./input.txt"
// Globals
let mutable rowSize = 0
let mutable columnSize = 0
let mutable start = (0,0)
let mutable finish = (0,0)
let setGlobals inp =
    rowSize <- inp |> Seq.head |> Seq.length
    columnSize <- inp |> Seq.length
    start <- Seq.findIndex2 'S' inp
    finish <- Seq.findIndex2 'E' inp
// Parsing
let parseChar = function
    | 'S' -> Parsing.abcToInt 'a'
    | 'E' -> Parsing.abcToInt 'z'
    | c -> Parsing.abcToInt c
let parseInput file =
    let data = file |> FileReading.readLines |> Seq.toList |> List.map seq
    setGlobals (data |> List.toSeq )
    data |> List.map Seq.toList |> List.map (List.map parseChar)
// Find edges
let getNear (data: int list list) ((x,y): int*int) = // Get all edges coming out of a point
    let h = data[y][x]
    let mutable near = []
    if(x>0 && data[y][x-1] <= h + 1) then near <- [(x,y),(x-1,y)]
    if(x<rowSize-1 && data[y][x+1] <= h + 1) then near <- ((x,y),(x+1,y))::near
    if(y>0 && data[y-1][x]<=h + 1) then near <- ((x,y),(x,y-1))::near
    if(y<columnSize-1 && data[y+1][x] <= h + 1) then near <- ((x,y),(x,y+1))::near
    near
let getEdges (inp: int list list) = // Determine all edges
    let mutable edges = []
    for y in [0..columnSize-1] do
        for x in [0..rowSize-1] do
            edges <- (getNear inp (x,y)) @ edges
    edges
let extraEdges (inp: int list list) = // Add edges pointing from an artificial start toward all other possible starts (Part 2)
    let mutable starts = []
    for y in [0..columnSize-1] do
        for x in [0..rowSize-1] do
            if (inp[y][x]=0) then starts <- (x,y)::starts
    starts |> List.map (fun s -> (-1,-1),s)
// Solving
let solve hasOneStart (inp: int list list) =
    let mutable edges = getEdges inp
    let mutable distances = Map.empty
    
    if hasOneStart then
        distances <- Map.empty<int*int,int>.Add (start,0)
    else
        edges <- extraEdges inp @ edges // Add extra edges for part 2
        distances <- Map.empty<int*int,int>.Add ((-1,-1),-1) // Set starting distance on -1 to make up for the eventual path to be +1 longer
    
    let mutable toConsider = distances
    
    while not toConsider.IsEmpty do
        let z = toConsider |> Seq.minBy (fun kv -> kv.Value ) |> fun kv -> kv.Key
        let outwards, edges' = edges |> Seq.splitBy (fun (z',_) -> z' = z)
        
        for _,z' in outwards do
            if (distances.ContainsKey z' |> not || 1 + distances.Item z < distances.Item z') then
                distances <- distances.Add (z', 1 + distances.Item z)
        
        edges <- edges' |> Seq.toList
        toConsider <-
            distances |> Map.filter (fun k _ -> Seq.exists (fun (z',_) -> z' = k) edges ) // Find vertices that currently have outgoing edges that are still to be considered
    
    distances.Item finish
// Solutions
let run hasOneStart = parseInput >> solve hasOneStart
let solution1 = run true file
let solution2 = run false file
printfn $"%A{solution1}"
printfn $"%A{solution2}"
// Solution1: 352
// Solution2: 345