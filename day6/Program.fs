open Utils

let input = "./input.txt"
let parseInput inp = inp |> FileReading.readLines |> Seq.reduce (+)
let isDistinct (str: seq<char>) =
    str
    |> Seq.distinct
    |> Seq.length
    |> (=) (Seq.length str)
let findMarker n (str: string) = // Finds first set of n distinct chars and returns index of the first char after that set
    str
    |> Seq.windowed n
    |> Seq.findIndex isDistinct
    |> (+) n
let run n = parseInput >> findMarker n
let solution1 = run 4 input
let solution2 = run 14 input
printfn $"Solution1: %i{solution1}"
printfn $"Solution1: %i{solution2}"
// Solution1: 1779
// Solution1: 2635
