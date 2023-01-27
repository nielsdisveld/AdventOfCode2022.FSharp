open Utils
let file = "./input.txt"    
// Scoring
let score1 = Seq.head
let score2 (calories: seq<int>) = (Seq.item 0 calories) + (Seq.item 1 calories) + (Seq.item 2 calories)
//Solution
let parseInput = FileReading.readLines >> Seq.splitAt (fun line -> line = "") >> Seq.map (Seq.map int)
let analyze score = Seq.map Seq.sum >> Seq.sortDescending >> score
let run f = parseInput >> analyze f
let solution1 = run score1 file
let solution2 = run score2 file
printfn $"Highest amount of calories is %i{solution1}"
printfn $"Sum of 3 highest amount of calories is %i{solution2}"

// Highest amount of calories is 70764
// Sum of 3 highest amount of calories is 203905