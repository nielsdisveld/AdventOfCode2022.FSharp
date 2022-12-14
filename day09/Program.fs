open Utils
let file = "./input.txt"
// Parsing
let parseLine (str: string) =
    match (str.Split ' ') with
    | [|direction;i|] -> direction,int i
    | _ -> failwith $"Invalid input: %A{str}"
let parseInput = FileReading.readLines >> Seq.map parseLine
// Helpers
let dz z1 z2 =
    match (z1 - z2) with
        | 0 -> 0
        | a -> a / (abs a) // normalize
// Update rope
let updateHead (x,y) = function
    | "R" -> x + 1,y
    | "U" -> x, y + 1
    | "L" -> x - 1,y
    | "D" -> x, y - 1
    | x -> failwith $"Invalid input: %A{x}"
let updateTail (x,y) (hx,hy) =
    if(abs (hx - x) <= 1 && abs (hy - y) <= 1) then x,y
    else x + dz hx x, y + dz hy y
// Solving
let solve n (input: seq<string*int>) =
    let start = 11,5
    let mutable rope = [0..n-1] |> List.map (fun _ -> start)
    let mutable visited = Set.singleton start
    
    for cmd, k in input do
        for _ in [1..k] do
            let mutable updatedRope = [updateHead rope[0] cmd]
            for i in [1..n-1] do
                updatedRope <- (updateTail rope[i] updatedRope[0])::updatedRope
            
            rope <- updatedRope |> List.rev
            visited <- visited.Add (rope.Item (n-1))      
    
    visited
// Analyzing
let analyze = Set.count
// Solution
let run n = parseInput >> solve n >> analyze
let solution1 = run 2 file
let solution2 = run 10 file
printfn $"solution1: %A{solution1}"
printfn $"solution2: %A{solution2}"
// Solution1: 5930