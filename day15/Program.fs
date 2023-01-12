open Utils
let file = "./input.txt"
// Helpers
let isInInterval x (i1,i2) = (x >= i1) && (x <= i2)
let manhattanDist (x1,y1) (x2,y2) = abs (x2-x1) + abs (y2-y1)
// Parsing
let intPrecedingChar (c: char) (str: string) = str.Split c |> Array.head |> int
let parseLine (str: string) =
    match str.Split '=' with
    | [|_;x1;y1;x2;y2|] ->
        (intPrecedingChar ',' x1, intPrecedingChar ':' y1), (intPrecedingChar ',' x2, int y2)
    | _ -> failwith $"Incorrect line: %A{str}"
let transformInput input =
    input
    |> Seq.map parseLine
    |> (Seq.map (fun (s,b) -> s, manhattanDist s b))
// Solving
let findNotCovered intervals =
    let rec loop x = 
        match intervals |> List.tryFind (isInInterval x) with
        | Some (_,i2) -> loop (i2+1)
        | None -> x
    loop 0
let getCover y0 ((sx,sy),d) =
    let dx = d - (abs (sy - y0))
    if dx < 0 then []
    else List.singleton (max 0 (sx-dx),min 4000000 (sx + dx))
let getCovers y0 inp =
    inp
    |> Seq.map (getCover y0)
    |> Seq.reduce List.append
let solve_y inp y =
    inp 
    |> getCovers y
    |> findNotCovered
    |> function
        | n when n <= 4000000 -> Some (n, y) // return x,y coordinate of point not covered
        | _ -> None
let solve (y_min,y_max) inp =
    [|y_min..y_max|]
    |> Array.tryPick (solve_y inp)
let solveParallel inp =
    [|0..3|]
    |> Array.map (fun i -> async {return solve (i*1000000,(i+1)*1000000) inp})
    |> Async.Choice
    |> Async.StartAsTask
    |> fun task -> task.Result
// Solutions
let analyze = function
    | Some (x,y) -> 4000000L*(int64 x) + (int64 y)
    | None -> failwith "Point not found"
let run () = file |> FileReading.readLines |> transformInput |> solveParallel |> analyze
let outcome = PerformanceTesting.timeOperation run
printfn $"Solution1: %A{outcome}"
// Solution1: 5127797
// Solution2: 12518502636475L speed: ~58.6s
