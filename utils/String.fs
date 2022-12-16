namespace Utils

module String =
    let splitStringAt (indexes: int list) (str: string) =
        let rec accumulator (indexes: int list) (str: string) (acc: string list) : string list =
            match indexes with
            | [] -> acc @ [str]
            | h::r -> accumulator (r |> List.map ((+) -(h+1))) str[h+1..] (acc @ [str[0..h-1]]) 
        accumulator (indexes |> List.sort) str []