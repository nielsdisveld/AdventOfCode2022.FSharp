open Utils

let file = "./input.txt"

type State = {Cycle: int; X: int}
// Parsing
let parseInput = FileReading.readLines
let updateState state (cmd: string) =
    match cmd.Split ' ' with
    | [|"noop"|] -> { state with Cycle = state.Cycle + 1 }
    | [|"addx"; i|] -> { Cycle = state.Cycle + 2; X = state.X + int i }
    | _ -> failwith $"%s{cmd}"
// Solving
let solve input =
    let mutable signal = List.empty
    let mutable pixels = List.empty
    let mutable state = {Cycle = 0; X = 1}
    
    for cmd in input do
        let newState = updateState state cmd
        for i in [state.Cycle+1..newState.Cycle] do
            if (i-1)%40>=state.X-1 && (i-1)%40<= state.X+1 then
                pixels <- i-1::pixels
            if (i - 20) % 40 = 0 then
                signal <- (i * state.X)::signal
            
        state <- newState    
    signal,pixels
// Analyzing      
let printLine (pixels: int list) = List.map (fun x -> if List.contains x pixels then "#" else ".") >> (List.reduce (+))
let printLines (pixels: int list) = [0..5] |> List.map (fun i -> [40*i..40*(i+1)-1]) |> List.map (printLine pixels)
// Solutions
let run f = parseInput >> solve >> f
let solution1 = run (fst >> Seq.sum) file
let solution2 = run (snd >> printLines) file
printfn $"%A{solution1}"
printfn $"%A{solution2}"
// Solution1: 11780
// Solution2: PZULBAUA