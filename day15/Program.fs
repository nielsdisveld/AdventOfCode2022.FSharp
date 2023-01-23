open Utils
let file = "./input.txt"
// Types
// Line segment is defined by starting point, slope (can only be 1 or -1 in this scope) and length
type LineSegment = (int * int) * int * int
// Helpers
let manhattanDist (x1,y1) (x2,y2) = abs (x2-x1) + abs (y2-y1)
// Parsing
let parseLine (str: string) =
    match str.Split [|'='; ','; ':'|] with
    | [|_;x1;_;y1;_;x2;_;y2|] -> (int x1, int y1), (int x2, int y2)
    | _ -> failwith $"Incorrect line: %A{str}"
let transformInput input =
    input
    |> Seq.map parseLine
    |> Seq.map (fun (s,b) -> s, manhattanDist s b)
// Line intersections
let intersectDescAsc (((x1,y1),_,d1): LineSegment)  (((x2,y2),_,d2): LineSegment) =
    match x1+y1+x2-y2 with // Basically follows from solving this equality: -(x-x1)+y1=(x-x2)+y2
    | x' when
        (x' % 2) = 0 && // If x' is odd then there is no integer solution
        x'/2>=x1 && x'/2<=(x1+d1) && x'/2>=x2 && x'/2<=x2+d2 -> [(x'/2, y1-(x'/2-x1))]
    | _ -> []
let rec intersection ((p1,slope1,d1): LineSegment, (p2,slope2,d2): LineSegment) =
    match slope1,slope2 with
    | -1,1 -> intersectDescAsc (p1,slope1,d1) (p2,slope2,d2)
    | 1,-1 -> intersectDescAsc (p2,slope2,d2) (p1,slope1,d1)
    | _ -> []
let toBoundaries ((x,y), d') : LineSegment[] = // Get line segments that define the boundary of what a sensor is covering
    let d = d'+1
    [|(x-d,y),1,d; (x,y+d),-1,d; (x-d,y),-1,d; (x,y-d),1,d;|]
// Solving
let analyze (x,y) = 4000000L*(int64 x) + (int64 y)
let allBoundaryIntersections (sensors: seq<(int*int)*int>) =
    sensors
    |> Seq.map toBoundaries // Map every sensor to its boundary
    |> fun s -> Seq.allPairs s s // Map to every combination of 2 boundaries
    |> Seq.map (fun (x,y) -> Seq.allPairs x y) // Map to every combination of 2 line segments
    |> Seq.collect (Seq.collect intersection) // Collect every possible intersection point of 2 boundaries
let findUncovered (sensors, intersections) =
    intersections
    |> Seq.find (fun (x,y) ->
        x>=0&&x<=4000000&&y>=0&&y<=4000000 &&
        sensors |> Seq.forall (fun (s,d) -> manhattanDist (x,y) s > d)) // Uncovered point is outside every sensor range
let solution =
    file
    |> FileReading.readLines
    |> transformInput
    |> fun sensors -> sensors, allBoundaryIntersections sensors
    |> findUncovered
    |> analyze
printfn $"%A{solution}"
// solution: 12518502636475