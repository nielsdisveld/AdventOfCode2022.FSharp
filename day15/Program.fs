open Utils
let file = "./input.txt"
// Helpers
let contains x (i1,i2) = x>=i1&&x<=i2
let rec findMissing intervals =
    let rec loop x  =
        match List.tryFind (contains x) intervals with
        | Some (_,i2) -> loop (i2+1)
        | None -> x
    loop 0
// Parsing
let parseInt (c: char) (str: string) = str.Split c |> Array.head |> int
let parseLine (str: string) =
    match str.Split '=' with
    | [|_;x1;y1;x2;y2|] ->
        (parseInt ',' x1, parseInt ':' y1), (parseInt ',' x2, int y2)
    | _ -> failwith $"Incorrect line: %i{(str.Split '=') |> Array.length}"
// Solving
let manhattanDist (x1,y1) (x2,y2) = abs (x2-x1) + abs (y2-y1)
let coveredAt y0 ((sx,sy),d) =
    let dx = d - (abs (sy - y0))
    if dx < 0 then []
    else List.singleton (max 0 (sx-dx),min 4000000 (sx + dx))
let solve0 y0 inp =
    inp
    |> Seq.map (coveredAt y0)
    |> Seq.reduce List.append
let solve inp =
    let distances =
        inp
        |> Seq.map (fun (s,b) -> s, manhattanDist s b)
    [|0..4000000|]
    |> Array.tryPick (fun i ->
        distances 
        |> solve0 i
        |> findMissing
        |> function
            | firstMiss when firstMiss <= 4000000 -> Some (firstMiss, i)
            | _ -> None)
let analyze = function
    | Some (x,y) -> 4000000L*(int64 x) + (int64 y)
    | None -> failwith "Point not found"
let run = file |> FileReading.readLines |> Seq.map parseLine |> solve |> analyze
let solution1 = run
printfn $"Solution1: %A{solution1}"
// Solution1: 5127797
// Solution2: 12518502636475L
