namespace Utils

open System

module Parsing =

    let charToInt (c: char) =
        match int c - int '0' with
        | i when i<=9 && i>=0 -> i
        | _ -> failwith $"Invalid integer: %c{c}"
    let abcToInt (c: char) = int c - int 'a'
    let rmvNonInts (str: string) =        
        str
        |> Seq.filter (fun n -> List.contains n ['0'..'9'])
        |> String.Concat