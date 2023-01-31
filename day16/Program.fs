open Utils
let file = "input.txt"
// Parsing
let parseCurrentValve (str: string) =
    match str.Split [|' ';'='|] with
    | [|_;currentValve;_;_;_;flowRate|] -> currentValve, int flowRate
    | _ -> failwith $"Invalid line part: %s{str}"
let parseConnectedValves (str: string) = str.Split ',' |> Array.map (fun s -> s.Trim())
let parseLine (str: string) =
    match str.Split "; tunnels lead to valves" with
    | [|currentValve; connectedValves|]-> parseCurrentValve currentValve, parseConnectedValves connectedValves
    | _ ->
        match str.Split  "; tunnel leads to valve" with
        | [|currentValve; connectedValves|]-> parseCurrentValve currentValve, parseConnectedValves connectedValves
        | _ -> failwith $"Invalid line: %s{str}"
// Transforming input
let edgesAndFlowRatesFolder (edges, flowRates) ((valve, flowRate), connectedValves) =
    connectedValves
    |> Array.map (fun connectedValve -> valve, connectedValve)
    |> Set.ofArray
    |> Set.union edges
    |> (fun set ->
        if flowRate = 0 then set // No point in opening a valve with zero flow rate. i.e. adding an edge from a valve to itself
        else Set.add (valve, valve) set)
    , flowRates |> Map.add valve flowRate
let edgesPerMin (v1,v2) = // We need to interpret each edge as movement + movement in time
    [|1..29|]
    |> Array.map (fun i -> (v1,i),(v2,i+1))
    |> Set.ofArray
let toAllEdgesPerMin = Set.map edgesPerMin >> Set.unionMany
let toNetReleasePerMin valve flowRate = [|1..30|] |> Array.map (fun i -> (valve,i),(31-i) * flowRate) // It is easier to translate flow rate to net release (which depends on the minute it was opened)
let toAllNetReleases (flowRates: Map<string,int>)=
    flowRates
    |> Seq.collect (fun kv -> toNetReleasePerMin kv.Key kv.Value)
    |> Map.ofSeq
let transformInput =
    FileReading.readLines
    >> Seq.map parseLine
    >> Seq.fold edgesAndFlowRatesFolder (Set.empty, Map.empty)
    >> fun (edges, vertices) -> toAllEdgesPerMin edges, toAllNetReleases vertices
// Solving
// We cannot just keep track of total release per vertex, we need to keep track of different sets of opened valves as well
type TotalReleases = Map<string*int, Map<Set<string>,int>> 
let moveToNextValve (nextValve: string*int) (currentReleases: TotalReleases) (openValves: Set<string>) (newRelease: int) =
    match currentReleases.TryFind nextValve with
    | None -> currentReleases.Add (nextValve, Map.empty.Add (openValves, newRelease))
    | Some map ->
        let currentRelease = map.TryFind openValves |> Option.defaultValue 0
        if currentRelease < newRelease then
            currentReleases.Add (nextValve, map.Add (openValves, newRelease))
        else currentReleases
let openValve ((valve,i): string*int) (toAdd: int) (currentReleases: TotalReleases) (openValves: Set<string>) (release: int) =
    match openValves.Contains valve, currentReleases.TryFind (valve,i) with
    | true, _ ->  currentReleases
    | _, None -> currentReleases.Add ((valve,i), Map.empty.Add (openValves.Add valve, release + toAdd))
    | _, Some map ->
        let currentRelease = map.TryFind (openValves.Add valve) |> Option.defaultValue 0
        if currentRelease < release + toAdd then
            currentReleases.Add ((valve,i), map.Add (openValves.Add valve, release + toAdd))
        else currentReleases
let updateTotalReleasesFolder (flowRates: Map<string*int,int>) (totalReleases: TotalReleases) ((v1,i),(v2,j)) =
    let currentReleasesAtEdgeSource = totalReleases.Item (v1,i)
    if v1 <> v2 then
        currentReleasesAtEdgeSource
        |> Map.fold (moveToNextValve (v2,j)) totalReleases
    else
        currentReleasesAtEdgeSource
        |> Map.fold (openValve (v2,j) (flowRates.Item (v2,j))) totalReleases
let solve (edges, flowRates: Map<string*int,int>)=
    let rec loop (releases: TotalReleases) edgesRemaining =
        let outgoingEdges = edgesRemaining |> Set.filter (fun (v,_) -> Seq.contains v releases.Keys)
        if outgoingEdges.IsEmpty then releases
        else
            let newReleases = outgoingEdges |> Set.fold (updateTotalReleasesFolder flowRates) releases
            let newRemaining = Set.difference edgesRemaining outgoingEdges
            loop newReleases newRemaining
    
    let initial = Map.empty.Add (("AA", 1), Map.empty.Add (Set.empty, 0))
    loop initial edges
// Solution
let analyze (totalReleases: TotalReleases) =
    totalReleases
    |> Seq.collect (fun kv ->
        kv.Value
        |> Seq.map (fun kv'-> kv'.Value))
    |> Seq.max
let run = transformInput >> solve >> analyze
let solution = run file
printfn $"%A{solution}"
// Solution1: 1741