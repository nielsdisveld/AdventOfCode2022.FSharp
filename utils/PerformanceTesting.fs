namespace Utils

module PerformanceTesting =
    open System.Diagnostics

    type TimedOperation<'T> = {millisecondsTaken:int64; returnedValue:'T}

    let timeOperation<'T> (func: unit -> 'T): TimedOperation<'T> =
        let timer = Stopwatch()
        timer.Start()
        let returnValue = func ()
        timer.Stop()
        {millisecondsTaken=timer.ElapsedMilliseconds; returnedValue=returnValue}

