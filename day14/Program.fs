open Utils
let file = "./input.txt"
// Types
type Coordinate = int*int
// Helpers
let generateArray n1 n2 = if n1 <= n2 then [|n1..n2|] else [|n2..n1|]
let add c p = c |> Set.add p
// Parsing
let parseCoordinate (str: string) : Coordinate =
    match str.Split ',' with
    | [|x;y|] -> (int x,int y)
    | _ -> failwith "Incorrect input %s{str}"
let drawPath (x1,y1) (x2,y2) =
    if x1 = x2 then generateArray y1 y2 |> Array.map (fun y -> x1,y)
    else  generateArray x1 x2 |> Array.map (fun x -> x,y1)
let parseLine (str: string) = 
    str.Split " -> "
    |> Array.map parseCoordinate
    |> Array.windowed 2
    |> Array.collect (fun coordinates -> drawPath coordinates[0] coordinates[1])
let parseInput =
    FileReading.readLines >>
    Seq.map parseLine >>
    Seq.collect id >>
    Set.ofSeq
// Solving
let source = (500,0)
let dropSand hasFloor floor caves =
    let rec loop (x0,y0) =
        [|x0,y0+1;x0-1,y0+1;x0+1,y0+1|]
        |> Seq.tryFind (fun cave -> not (Set.contains cave caves))
        |> function
            | None when (x0,y0) = source -> add caves source, false
            | None -> add caves (x0,y0), true
            | Some (x,y) ->
                if not hasFloor && y = (floor + 1) then caves, false
                elif y = (floor + 1) then add caves (x,y), true
                else loop (x,y)
    loop source
let dropStream hasFloor rocks = 
    let floor = rocks |> Seq.maxBy snd |> snd
    let rec loop sand =
        match dropSand hasFloor floor sand with
        | sand', true -> loop sand'
        | sand', false -> sand'

    Seq.length (loop rocks) - (Seq.length rocks)
let solve1 = dropStream false
let solve2 input = dropStream true input
// Solutions
let analyze = Seq.length
let run f = parseInput >> f 
let solution1 = run solve1 file
printfn $"Solution1: %A{solution1}"
let solution2 = run solve2 file
printfn $"Solution2: %A{solution2}"
// Solution1: 795
// Solution2: 30214
