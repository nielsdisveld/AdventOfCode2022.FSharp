open Utils
let file = "./input.txt"
// Line parsing
let halve (str: string) =
    let half = str.Length / 2
    str[..half-1], str[half..]
// Find items
let findError (str1: string, str2: string) = str1|> Seq.tryFind str2.Contains
let findBadge (str1: string,str2: string,str3: string) = str1 |> Seq.filter str2.Contains |> Seq.tryFind str3.Contains
let toTriple = function
    | [|a;b;c|] -> a,b,c
    | x -> failwith $"%A{x}"
// Priorities
let scoreChar (c: char) =
    match int c with
    | i when i > 96 -> i - 96
    | i -> i - 38
let priority = function
    | None -> 0
    | Some i -> scoreChar i
// Solutions
let parseInput = FileReading.readLines
let analyze = Seq.map priority >> Seq.sum
let run f = parseInput >>  f >> analyze
let solve1 = Seq.map (halve >> findError) // Halve every line in 2 groups and find duplicate
let solve2 = Seq.chunkBySize 3 >> Seq.map (toTriple >> findBadge) // Create distinct groups of 3 and find duplicate
let solution1 = run solve1 file
let solution2 = run solve2 file

printfn $"%i{solution1}"
printfn $"%i{solution2}"
// Solution1: 8515
// Solution2: 2434
