namespace Utils

module Parsing =

    let charToInt (c: char) =
        match int c - int '0' with
        | i when i<=9 && i>=0 -> i
        | _ -> failwith $"Invalid integer: %c{c}"