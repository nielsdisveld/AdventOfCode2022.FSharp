open Utils
let file = "./input.txt"    
// Line parsing
let parseLine = function
    | "" -> None
    | x -> Some (int x)
let folder (state: list<list<int>>) (intOpt: int option) : list<list<int>> =
    match state, intOpt with
    | [], None -> []
    | t, None -> []::t
    | [], Some i -> [[i]]
    | h::t, Some i -> (i::h)::t
// Scoring
let score1 = List.head
let score2 (calories: int list) = calories[0..2] |> List.sum
//Solution
let parseInput = FileReading.readLines >> Seq.map parseLine >> Seq.toList
let analyze score = List.map List.sum >> List.sortDescending >> score
let run f = parseInput >> List.fold folder [] >> analyze f
let solution1 = run score1 file
let solution2 = run score2 file
printfn $"Highest amount of calories is %i{solution1}"
printfn $"Sum of 3 highest amount of calories is %i{solution2}"

// Highest amount of calories is 70764
// Sum of 3 highest amount of calories is 203905
