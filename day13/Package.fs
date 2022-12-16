module Package
    
    open System
    
    [<CustomComparison; CustomEquality>]
    type Package =
        | Value of int
        | List of Package list

        member this.compare self other : int =
            match self, other  with
                | List [], List[] -> 0
                | List [], _ -> -1
                | _, List [] -> 1
                | List (h1::r1), List (h2::r2) ->
                    match this.compare h1 h2 with
                    | 0 -> this.compare (List r1) (List r2)
                    | i -> i
                | Value v1, Value v2 -> v1 - v2
                | Value v, List l -> this.compare (List [Value v]) (List l)
                | List l, Value v -> this.compare (List l) (List [Value v])
        override this.GetHashCode() =
            match this with
            | Value v -> hash v
            | List [] -> 0
            | List (h::t) -> h.GetHashCode () + t.GetHashCode ()
        override this.Equals other =
            match other with
            | :? Package as p -> (this.compare this p) = 0
            | _ -> false
        interface IComparable with
            member this.CompareTo other =
                match other with
                | :? Package as p -> (this :> IComparable<_>).CompareTo p
                | _ -> -1
        interface IComparable<Package> with
            member this.CompareTo other = this.compare this other
        interface IEquatable<Package> with
            member this.Equals other = this.compare this other = 0