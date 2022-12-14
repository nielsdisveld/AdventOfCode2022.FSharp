open Utils
let file = "./input.txt"    
// Input parsing
let parseChar = function
    | 'A' | 'X' -> 1
    | 'B' | 'Y' -> 2
    | 'C' | 'Z' -> 3
    | x -> failwith $"Invalid character input %A{x}"
let determineP2Response = function
    | 'A', 'X' | 'C', 'Y' | 'B', 'Z' -> 3
    | 'B', 'X' | 'A', 'Y' | 'C', 'Z' -> 1
    | 'C', 'X' | 'B', 'Y' | 'A', 'Z' -> 2
    | x1,x2 -> failwith $"Invalid character input %A{x1}, %A{x2}" 
let parseLine secondColParse lineInput =
    match lineInput with
    | [|p1;_;p2|] -> (parseChar p1, secondColParse (p1,p2))
    | x -> failwith $"Invalid line input %A{x.ToString}"
// Scoring
let roundScore = function
    | p1,p2 when p1 = p2 -> 3
    | p1,p2 when p2 = p1 + 1 -> 6
    | 3,1 -> 6
    | _ -> 0
let scoreLine (p1,p2) = roundScore (p1,p2)  + p2
// Solution
let analyze = Seq.map scoreLine >> Seq.sum
let run secondColParser = FileReading.readLines >> Seq.map (Seq.toArray >> parseLine secondColParser) >> analyze
let solution1 = run (snd >> parseChar) file
let solution2 = run determineP2Response file

printfn $"Solution 1: Total score for player 2: %i{solution1}"
printfn $"Solution 2: Total score for player 2: %i{solution2}"

// Solution 1: Total score for player 1: 17189
// Solution 2: Total score for player 1: 13490