open Utils
let file = "./input.txt"
// Types
type Caves = Map<int,Set<int>> // keys are x coordinates, a value is the set of all possible y values for an x
// Helpers
let generateArray n1 n2 = if n1 <= n2 then [|n1..n2|] else [|n2..n1|]
let addCave (m: Caves) (x,y) =
    m.TryFind x
    |> Option.defaultValue Set.empty
    |> Set.add y
    |> fun s -> m.Add (x,s)
let contains (x,y) (m: Caves) =
    match m.TryFind x with
    | Some s -> Set.contains y s
    | None -> false
let count (caves: Caves) = caves.Values |> Seq.sumBy Set.count 
// Parsing
let parseCoordinate (str: string)  =
    match str.Split ',' with
    | [|x;y|] -> (int x,int y)
    | _ -> failwith "Incorrect input %s{str}"
let drawPath (x1,y1) (x2,y2) =
    if x1 = x2 then generateArray y1 y2 |> Array.map (fun y -> x1,y)
    else generateArray x1 x2 |> Array.map (fun x -> x,y1)
let parseLine (str: string) = 
    str.Split " -> "
    |> Array.map parseCoordinate
    |> Array.windowed 2
    |> Array.collect (fun coordinates -> drawPath coordinates[0] coordinates[1])
let transformInput =
    Seq.map parseLine >>
    Seq.collect id >>
    Seq.groupBy fst >>
    Seq.map (fun (k,l) -> k, Seq.map snd l |> set) >>
    Map.ofSeq
// Solving
let source = (500,0)
let dropSandUnit hasFloor floor (caves: Caves) =
    let rec loop (x0,y0) =
        [|x0,y0+1;x0-1,y0+1;x0+1,y0+1|]
        |> Seq.tryFind (fun cave -> not (contains cave caves))
        |> function
            | None when (x0,y0) = source -> addCave caves source, false
            | None -> addCave caves (x0,y0), true
            | Some (x,y) ->
                if not hasFloor && y = (floor + 1) then caves, false
                elif y = (floor + 1) then addCave caves (x,y), true
                else loop (x,y)
    loop source
let dropSandStream hasFloor (rocks: Caves) = 
    let floor = rocks.Values |> Seq.collect id |> Seq.max
    let rec loop sand =
        match dropSandUnit hasFloor floor sand with
        | caves', true -> loop caves'
        | caves', false -> caves'
    count (loop rocks) - (count rocks)
let solve1 input = dropSandStream false input
let solve2 input = dropSandStream true input
// Solutions
let run f = FileReading.readLines >> transformInput >> f 
let solution1 = run solve1 file
printfn $"Solution1: %A{solution1}"
let solution2 = run solve2 file
printfn $"Solution2: %A{solution2}"
// Solution1: 795
// Solution2: 30214, speed: ~1666ms
