open System
open Package
open Utils
let file = "./input.txt"
// Parsing
let parseInput = FileReading.readLines
// Package parsing
let getRootList (str: seq<char>) =
    let mutable openLists = 0
    let mutable separators = [0;Seq.length str - 1]
    let mutable i = 0
    for c in str do
        match c with
        | '[' -> openLists <- openLists + 1
        | ']' -> openLists <- openLists - 1
        | ',' -> if openLists = 1 then separators <- separators @ [i]
        | _ -> ()
        i <- i+1
    str |> String.Concat |> String.splitStringAt separators |> List.filter ((<>) "")
let rec parsePackage (str: string) : Package =
    if List.contains str[0] ['1'..'9'] then
        (Value (int str))
    else 
        let lst = getRootList str
        lst |> List.map parsePackage |> List   
// Solving
let solve1 inp =
    let pairs = inp |> Seq.chunkBySize 3 |> Seq.map (fun c -> parsePackage c[0], parsePackage c[1])
    let mutable correct = []
    let mutable i = 1
    for package1, package2 in pairs do
        if package1 <= package2 then correct <- i::correct
        i <- i + 1
    correct |> List.sum
let solve2 inp =
    let dividers = [parsePackage "[[2]]"; parsePackage "[[6]]"]
    let parsed = inp |> Seq.filter ((<>) "") |> Seq.map parsePackage
    let sorted = Seq.concat [parsed; dividers] |> Seq.sort
    let mutable indexes = []
    let mutable i = 1
    for package in sorted do
        if List.contains package dividers then indexes <- i::indexes
        i <- i + 1
    indexes |> List.reduce (*)
// Solutions
let run f = parseInput >> f
let solution1 = run solve1 file
let solution2 = run solve2 file
printfn $"%A{solution1}"
printfn $"%A{solution2}"
// Solution1: 5390
// Solution2: 19261