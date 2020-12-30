module aoc2015.day_10

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

open FsUnit.Xunit
open Xunit
open System.Text.RegularExpressions


let source = File.load 10

let puzzle_1 = 0

let puzzle_2 = 0

let rx = Regex @"(\d)\k<1>*"

let stringify (m: Match) =
    sprintf "%i%c" m.Value.Length m.Value.[0]

let look_and_say s =
    rx.Matches s
    |> Seq.map stringify
    |> String.concat ""


let rec find_1 s =
    function
    | 0 -> String.length s
    | n -> find_1 (look_and_say s) (n - 1)


[<Fact>]
let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false


[<Fact>]
let ``puzzle 1 is correct`` () =
    find_1 "1321131112" 40 |> should equal 492982


[<Fact>]
let ``puzzle 2 is correct`` () =
    find_1 "1321131112" 40 |> should equal 1321131112
