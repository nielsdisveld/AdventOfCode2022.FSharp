open System
open Package
open Utils
let file = "./input.txt"
// Parsing
let parseInput = FileReading.readLines
// Package parsing
let getSeparators (str: seq<char>) =
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
    separators
let rec parsePackage (str: string) : Package =
    if List.contains str[0] ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9'] then (Value (int str))
    else 
        let separators = getSeparators str
        let seperated = str |> Seq.mapi (fun i c -> if List.contains i separators then '|' else c) |> String.Concat
        let lst2 = seperated.Split '|' |> Array.toList |> List.filter ((<>) "")
        lst2 |> List.map parsePackage |> List   
// Solving
let solve inp =
    let pairs = inp |> Seq.chunkBySize 3 |> Seq.map (fun c -> parsePackage c[0], parsePackage c[1])
    let mutable correct = []
    let mutable i = 1
    for package1, package2 in pairs do
        if package1 <= package2 then correct <- i::correct
        i <- i + 1
    correct
// Analyzing
let analyze = List.sum
// Solutions
let run = parseInput >> solve >> analyze
let solution1 = run file
printfn $"%A{solution1}"
// Solution1: 5390