open Utils
let file = "./input.txt"
// Input parsing
let parseLine (str: string) =
    match str.Split [|',';'-'|] with
    | [|i1;i2;i3;i4|] -> ((int i1,int i2),(int i3,int i4))
    | _ -> failwith $"Invalid line input: %s{str}" 
// Interval check helpers
let containsOther ((i1,i2),(i3,i4)) = (i1 <= i3 && i2>=i4) || (i1 >= i3 && i2<=i4)
let hasNoOverlap ((i1,i2),(i3,i4)) = i2 < i3 || i1 > i4
//Solution
let parseInput = FileReading.readLines >> Seq.map parseLine
let solve filter = Seq.filter filter
let analyze = Seq.length
let run filter = parseInput >> solve filter >> analyze
let solution1 = run containsOther file
let solution2 = run (hasNoOverlap >> not) file

printfn $"%A{solution1}"
printfn $"%A{solution2}"
// solution 1: 513
// solution 2: 878