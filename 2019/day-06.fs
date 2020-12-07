module aoc2019.day_06

open FsUnit.Xunit
open Xunit
open Lib

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 06 

[<Fact>]
let ``have source file`` () =  source |> should not' (equal Seq.empty)

let toOrbit (s: string) =
    match s.IndexOf(')') with
    | -1 -> None
    | i -> (s.Substring(0, i), s.Substring(i+1)) |> Some

let directOrbits map planet =
    map
    |> Seq.tryFind (snd >> (=) planet)
    |> Option.map fst

let rec getOrbits map planet =
    match directOrbits map planet with
    | None -> []
    | Some "COM" -> [ "COM" ]
    | Some p -> p :: (getOrbits map p)

let first source =
    let map = source |> Seq.choose toOrbit

    map
    |> Seq.map snd
    |> Seq.distinct
    |> Seq.sumBy (getOrbits map >> List.length)
    // |> Seq.map (getOrbits map >> List.length)
    // |> Seq.sum

let commonOrbit a b =
    seq {
        for x in a do
            for y in b do
                if x = y then yield x
    }
    |> Seq.head

let indexOf m p =
    m
    |> Seq.indexed
    |> Seq.find (snd >> (=) p)
    |> fst


let second source = 
    let map = source |> Seq.choose toOrbit
    let toYou = getOrbits map "YOU"
    let toSan = getOrbits map "SAN"
    let common = commonOrbit toYou toSan
    (indexOf toYou common) + (indexOf toSan common)

[<Theory>]
[<InlineData("COM)B B)C C)D D)E E)F B)G G)H D)I E)J J)K K)L", 42)>]
let testFirst (source: string, expected) =
    let actual = source.Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries) |> first
    Assert.Equal(expected, actual)

[<Fact(Skip = "Long runtime")>]
let evaluate() = 
    Assert.Equal(251208, first source)
    Assert.Equal(397, second source)
