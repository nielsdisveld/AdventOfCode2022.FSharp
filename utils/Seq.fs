namespace Utils

module Seq =
    let add seq s = Seq.append seq (Seq.singleton s)
    let zipzip seq1 seq2 = Seq.zip seq1 seq2 |> Seq.map (fun (a,b) -> Seq.zip a b)
    let map4 f seq1 seq2 seq3 seq4 : seq<seq<_>>=
        zipzip (zipzip seq1 seq2) (zipzip seq3 seq4)
        |> Seq.map (Seq.map (fun ((x1,x2),(x3,x4)) -> f x1 x2 x3 x4))
    let actOnRev f = Seq.rev >>  f >> Seq.rev
    let actOnTranspose f = Seq.transpose >> f >> Seq.transpose
    let pointWise f (inp: seq<seq<_>>) = inp |> Seq.map2 (Seq.map2 f)
    let splitBy f (inp: seq<_>) =
        let split = inp
                    |> Seq.groupBy f
        let pos = split // Entries which were valuated positively by f
                  |> Seq.tryFind fst
                  |> Option.map snd
                  |> Option.defaultValue Seq.empty
        let neg = split// Entries which were valuated negatively by f
                  |> Seq.tryFind (fst >> not)
                  |> Option.map snd
                  |> Option.defaultValue Seq.empty
        pos, neg
    let findIndex2<'a when 'a: equality> (v: 'a) (inp: seq<seq<'a>>) =
        let y = inp |> Seq.findIndex (Seq.contains v)
        let x = Seq.item y inp |> Seq.findIndex (fun item -> v = item)
        (x,y)