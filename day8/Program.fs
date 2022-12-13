open Utils

let file = "./input.txt"
// Parsing
let parseLine: string -> seq<int> = Seq.map Parsing.charToInt 
let parseInput = FileReading.readLines >> Seq.map parseLine
// Solve is visible
let isVisible height (halfLine: seq<int>) =
    if Seq.isEmpty halfLine then true
    else halfLine |> Seq.forall ((>) height)
let isVisible1d (line: seq<int>) i =
    let height = Seq.item i line
    (Seq.take i line |> Seq.rev , Seq.skip (i+1) line)
    ||> fun a b -> (isVisible height a) || (isVisible height b)
let isVisibleLines =
    Seq.map
        (fun line ->
        [0..Seq.length line-1]
        |> Seq.map (isVisible1d line))
let findVisible input =
    (input |> isVisibleLines, input |> Seq.actOnTranspose isVisibleLines)
    ||> Seq.pointWise (||)
// Solve can see
let canSee height (halfLine: seq<int>) =
    if Seq.isEmpty halfLine then 0
    else
        halfLine
        |> Seq.tryFindIndex ((<=) height)
        |> Option.map ((+) 1) 
        |> Option.defaultValue (Seq.length halfLine)
let canSee1d (line: seq<int>) i =
    let height = Seq.item i line
    (Seq.take i line |> Seq.rev , Seq.skip (i+1) line)
    ||> fun a b -> (canSee height a) * (canSee height b)
let canSeeLines =
    Seq.map
        (fun line ->
        [0..Seq.length line - 1]
        |> Seq.map (canSee1d line))
let findCanSee (input: seq<seq<int>>) =
    (input |> canSeeLines, input |> Seq.actOnTranspose canSeeLines)
    ||> Seq.pointWise (*)
// Analyzing
let analyze : seq<seq<bool>> -> int = Seq.sumBy ((Seq.filter id) >> Seq.length)
let analyze2 = (Seq.map Seq.max) >> Seq.max
// Solutions
let run = parseInput >> findVisible >> analyze
let run2 = parseInput >> findCanSee >> analyze2
let solution1 = run file
let solution2 = run2 file
printfn $"%i{solution1}"
printfn $"%i{solution2}"
// Solution1: 1809
// Solution2: 479400
