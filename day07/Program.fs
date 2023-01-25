open Utils
let file = "./input.txt"
// Types
type Command = | CdRoot | CdOut | Cd of string | Ls
type File = int64
type Dir = string
type Line = | Command of Command | File of File | Dir of Dir
type State = { sizes: Map<string,int64>; dirs: string list } // Needed for folder function
// Parsing
let parseCommand = function
    | ["ls"] -> Ls
    | ["cd";".."] -> CdOut
    | ["cd";"/"] -> CdRoot
    | ["cd";c] -> Cd (c+"/")
    | x -> failwith $"Invalid command %A{x}"
let parseLine (str: string) : Line =
    match str.Split ' ' |> Array.toList with
    | "$"::xs -> parseCommand xs |> Line.Command
    | ["dir";d] -> Dir d
    | [s;_] -> File (int64 s)
    | x -> failwith $"Invalid line %A{x}"
let parseInput = FileReading.readLines >> Seq.map parseLine
// Helpers functions
let add n (map: Map<_,_>) k =
    map.TryFind k
    |> Option.defaultValue 0L
    |> fun n' -> map.Add (k, n'+n)
let toAbsolutePaths = // map each dir to its absolute path
    List.rev
    >> List.fold (fun (lst, absPath) dir -> (absPath+dir::lst, absPath+dir)) ([], "")
    >> fst
// Solving
let handleCmd (state: State) = function
    | CdRoot -> {state with dirs = ["/"]}
    | CdOut -> { sizes = state.sizes; dirs = state.dirs |> List.tail}
    | Cd dirName -> {state with dirs = dirName::state.dirs}
    | Ls -> state
let updateSizes (state: State) (size: int64) =
    { state with sizes =
                    state.dirs
                    |> toAbsolutePaths
                    |> List.fold (add size) state.sizes }
let folder (state: State) = function
    | Command cmd -> cmd |> handleCmd state
    | File size -> size |> updateSizes state
    | _ -> state
let solve =
    Seq.fold folder { sizes = Map.empty; dirs = ["/"]}
    >> (fun s -> s.sizes.Values)
// Analyzing
let analyze1 = Seq.filter ((>=) 100000L) >> Seq.sum
let analyze2 sizes =
    let totalUsed = sizes |> Seq.max
    sizes
    |> Seq.filter (fun s -> (s-totalUsed) >= (30000000L-70000000L))
    |> Seq.min
// Solutions
let run f = parseInput >> solve >> f
let solution1 = run analyze1 file
let solution2 = run analyze2 file
printfn $"Solution1: %A{solution1}"
printfn $"Solution1: %A{solution2}"
// Solution1: 1453349
// Solution2: 2948823