open Utils
let file = "./input.txt"
// Types
type Coordinate = int*int
// Helpers
let flip tuple = snd tuple, fst tuple
// Parsing
let parseCoordinate (str: string) : Coordinate =
    match str.Split ',' with
    | [|x;y|] -> (int x,int y)
    | _ -> failwith "Incorrect input %s{str}"
let drawPath (x1,y1) (x2,y2) =
    match [|x1..x2|] with
    | [|x1|] -> [|y1..y2|] |> Array.map (fun y -> x1,y)
    | xs -> xs |> Array.map (fun x -> x,y1)
let parseLine (str: string) = 
    str.Split " -> "
    |> Array.map parseCoordinate
    |> Array.windowed 2
    |> Array.map (fun coordinates -> drawPath coordinates[0] coordinates[1])
    |> Array.collect id
let parseInput = FileReading.readLines >> Seq.map parseLine >> Seq.collect id >> Seq.distinct >> Seq.sortBy flip >> Seq.toList
// Solving
let source: Coordinate = (0,500)
let rec moveSand (unitsOfSand: int) (sandUnit: Coordinate) (obstacles: Coordinate list) =
    match obstacles |> List.tryFind (fun (_,y) -> y = snd sandUnit) with
    | Some (x,y) ->
        if List.contains (x-1,y+1) obstacles then
            moveSand unitsOfSand (x-1,y+1) obstacles
        elif List.contains (x+1,y+1) obstacles then
            moveSand unitsOfSand (x+1,y+1) obstacles
        else
            (x,y)::obstacles
            |> moveSand (unitsOfSand+1) source
    | None -> unitsOfSand
let solve = moveSand 0 source
// Solutions
let run = parseInput >> solve
let solution1 = run file
printfn $"Solution1: %A{solution1}"