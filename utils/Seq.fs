namespace Utils

module Seq =
    let append seq s = Seq.append seq (Seq.singleton s)
    let zipzip seq1 seq2 = Seq.zip seq1 seq2 |> Seq.map (fun (a,b) -> Seq.zip a b)
    let map4 f seq1 seq2 seq3 seq4 : seq<seq<_>>=
        zipzip (zipzip seq1 seq2) (zipzip seq3 seq4)
        |> Seq.map (Seq.map (fun ((x1,x2),(x3,x4)) -> f x1 x2 x3 x4))
    let actOnRev f = Seq.rev >>  f >> Seq.rev
    let actOnTranspose f = Seq.transpose >> f >> Seq.transpose
    let pointWise f (inp: seq<seq<_>>) = inp |> Seq.map2 (Seq.map2 f)

