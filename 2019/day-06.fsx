open System
open System.IO


let pairs (s: string) =
    match s.IndexOf(')') with
    | -1 -> None
    | i -> Some(s.Substring(0, i), s.Substring(i + 1))

let parse (s: string) = s.Split(Environment.NewLine.ToCharArray()) |> Seq.choose pairs

let planetsFrom map =
    map
    |> Seq.map snd
    |> Seq.distinct

let orbit map planet =
    map
    |> Seq.tryFind (snd >> (=) planet)
    |> Option.map fst

let rec countOrbits map planet =
    match orbit map planet with
    | None -> 0
    | Some "COM" -> 1
    | Some p -> (countOrbits map p) + 1

let rec getOrbits map planet =
    match orbit map planet with
    | None -> []
    | Some "COM" -> [ "COM" ]
    | Some p -> p :: (getOrbits map p)

let map =
    Path.Combine(__SOURCE_DIRECTORY__, "day-06.txt")
    |> File.ReadLines
    |> Seq.choose pairs
    |> Seq.cache


let planets =
    map
    |> Seq.map snd
    |> Seq.distinct

let a1 =
    planets
    |> Seq.map (countOrbits map)
    |> Seq.sum


let common a b =
    seq {
        for x in a do
            for y in b do
                if x = y then yield x
    }
    |> Seq.head

let dist m p =
    m
    |> Seq.indexed
    |> Seq.find (snd >> (=) p)
    |> fst

let transfers map =
    let toYOU = getOrbits map "YOU"
    let toSAN = getOrbits map "SAN"
    let c = common toSAN toYOU
    (dist toYOU c) + (dist toSAN c)

let a2 = transfers map
