namespace Utils

open System

module Parsing =

    let charToInt (c: char) =
        match int c - int '0' with
        | i when i<=9 && i>=0 -> i
        | _ -> failwith $"Invalid integer: %c{c}"
    let abcToInt (c: char) = int c - int 'a'
    let rmvLastChar (str: string) =
        if Seq.length str = 0 then failwith "Cannot remove last char from empty string"
        str
        |> Seq.rev
        |> Seq.tail
        |> Seq.rev
        |> String.Concat
    let rmvNonInts (str: string) =        
        str
        |> Seq.filter (fun n -> List.contains n ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9'])
        |> String.Concat