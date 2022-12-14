open Utils
let file = "./input.txt"
// Types
type Input = | Val of int64 | Old
type Operation = Input * string * Input
type Monkey = { Items: int64 list; Operation: Operation; Test: int64; IfTrue: int; IfFalse: int }
let emptyMonkey = { Items = []; Operation = (Val 0,"",Val 0); Test = 0; IfTrue = 0; IfFalse = 0 }
// Parsing
let parseOpInput = function
    | "old" -> Old
    | i -> Val (int64 i)
let updateMonkey (monkey: Monkey) = function
    | "Starting"::_::r -> { monkey with Items = r |> List.map Parsing.rmvNonInts |> List.map int64 }
    | "Operation:"::_::_::inp1::op::[inp2] -> {monkey with Operation = parseOpInput inp1, op, parseOpInput inp2 }
    | "Test:"::_::_::[n] -> { monkey with Test = int n }
    | "If"::"true:"::_::_::_::[n] -> { monkey with IfTrue = int n }
    | "If"::"false:"::_::_::_::[n] -> { monkey with IfFalse = int n }
    | str -> failwith $"Invalid line: %A{str}"
let parseLine (state: Monkey list) (str: string) =
    match str.Split ' ' |> Seq.toList |> List.filter ((<>) "") with
    | [] -> state
    | ["Monkey"; _] -> emptyMonkey :: state
    | cmd -> updateMonkey state.Head cmd :: state.Tail
let toMap = Seq.mapi (fun i m -> i,m) >> Map.ofSeq
let parseInput = FileReading.readLines >> Seq.fold parseLine [] >> Seq.rev >> toMap
// Solving
let containWorry1 _ i = i / 3L
let containWorry2 m i = i % m
let setVal i = function | Old -> i | Val j -> j
let updateWorry i (val1, op, val2) =
    match op with
    | "*" -> (setVal i val1) * (setVal i val2)
    | "+" -> (setVal i val1) + (setVal i val2)
    | x -> failwith $"Incorrect operator: %s{x}"
let addItem (monkey: Monkey) i = { monkey with Items = List.append monkey.Items [i] }
let removeItems (monkey: Monkey) = { monkey with Items = [] }
let solve f n (monkeys0: Map<int,Monkey>) =
    let mutable monkeys = monkeys0
    let mutable inspected = monkeys.Keys |> Seq.map (fun i -> i,0L) |> Map.ofSeq
    let commonProd = monkeys |> Seq.map (fun kv -> kv.Value.Test) |> Seq.reduce (*)
    let containWorry = f commonProd
    for _ in [0..n-1] do
        for i in monkeys.Keys do
            let monkey = monkeys[i]
            for item in monkey.Items do
                inspected <- inspected.Add (i,inspected[i]+1L)
                let updatedItem = updateWorry item monkey.Operation |> containWorry
                if((updatedItem % monkey.Test) = 0L) then
                    monkeys <- monkeys.Add (monkey.IfTrue, addItem monkeys[monkey.IfTrue] updatedItem)
                else
                    monkeys <- monkeys.Add (monkey.IfFalse, addItem monkeys[monkey.IfFalse] updatedItem)
            monkeys <- monkeys.Add (i, removeItems monkey)
    inspected
// Analyzing
let analyze = Map.values >> Seq.sortDescending >> (fun s -> (Seq.item 0 s) * (Seq.item 1 s))
let run f n = parseInput >> solve f n >> analyze
// Solutions
let solution1 = run containWorry1 20 file
let solution2 = run containWorry2 10000 file
printfn $"%A{solution1}"
printfn $"%A{solution2}"
// Solution1: 108240
// Solution2: 25712998901