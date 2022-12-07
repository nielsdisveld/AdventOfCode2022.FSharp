open Utils

type Stacks = char list list
// Input
let input = "./input.txt"
let splitInput (input: seq<string>) =
    let indexToSplit = Seq.findIndex (fun (s: string) -> s.Equals("")) input // find empty line
    let noOfStacks = input |> Seq.item (indexToSplit - 1) |> Seq.rev |> Seq.item 1 |> (string >> int) // find number of stacks
    input |> Seq.take (indexToSplit - 1), noOfStacks - 1, input |> Seq.skip (indexToSplit + 1) // return the crates part, parsed number of stacks, movements part
// Updating stacks
let addCrateToStack (stacks: Stacks) = function
    | _, ' ' -> stacks
    | i, c -> stacks |> List.updateAt i (stacks[i]@[c])
// Crate input parsing
let parseCrateLine n (stacks: Stacks) (str: string) =
    [0..n]
    |> List.map (fun i -> i, str[4*i + 1])
    |> List.fold addCrateToStack stacks
let emptyStacks n = [0..n] |> List.map (fun _ -> [])
let parseCrateLines n stacksInput = stacksInput |> Seq.fold (parseCrateLine n) (emptyStacks n)
// Movement input parsing
let parseMovementLine (str: string) = str.Split ' ' |> fun s -> int s[1], int s[3] - 1, int s[5] - 1 // Get the relevant integers from the movement line
// Parse all
let parseInput =
    FileReading.readLines >> splitInput >>
    fun (crates,n,movements) ->
        parseCrateLines n crates,
        movements |> Seq.map parseMovementLine 
// Crate re-arranging
let rearrange f (stacks: Stacks) ((n, s1, s2): int*int*int) : Stacks = // folder function
    let cratesToMove = stacks |> List.item s1 |> List.take n |> f
    stacks
    |> List.updateAt s1 (List.skip n stacks[s1])
    |> List.updateAt s2 (cratesToMove@stacks[s2])
let rearrangeAll f (crates, movements) = movements |> Seq.fold (rearrange f) crates
// Solution
let analyze = List.map (List.head >> string) >> List.reduce (+)
let run f = parseInput >> rearrangeAll f >> analyze // f is basically id or List.rev depending on the type of crane
let solution1 = run List.rev input
let solution2 = run id input
 
printfn $"%A{solution1}"
printfn $"%A{solution2}"

// Solution1: TPGVQPFDH
// Solution2: DMRDFRHHH