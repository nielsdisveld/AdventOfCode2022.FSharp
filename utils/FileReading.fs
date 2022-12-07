namespace Utils

open System.IO

module FileReading =
    let readLines (filePath: string) = seq {
        let reader = new StreamReader (filePath)
        while not reader.EndOfStream do 
            yield reader.ReadLine ()}