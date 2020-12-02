module Puzzles.day_02

open Xunit
open Lib
open System.Text.RegularExpressions

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

open FsUnit.Xunit

let source = File.load 2 |> Seq.filter (System.String.IsNullOrEmpty >> not)

let rx = Regex(@"(\d+)-(\d+) ([a-z]): (.*\3.*)")

let mapping line = 
    (rx.Match line).Groups
    |> Seq.cast<Group> 
    |> Seq.skip 1 
    |> Seq.map (fun g -> g.Value) 
    |> Seq.toList 
    |> function | a::b::c::d::_ -> Some ((int) a, (int) b,c, d.Trim()) | _ -> None
    
let isvalid_1 (a, b, c, d) = 
    let count  = Regex.Matches(d, c).Count
    count >= a && count <= b

let isvalid_2 (a, b, c, (d:string)) =
    let a' = d.[a-1].ToString() = c
    let b' = d.[b-1].ToString() = c
    (a' || b') && (a' <> b')

let data =
    [ "1-3 a: abcde"
      "1-3 b: cdefg"
      "2-9 c: ccccccccc" ]
    |> List.choose mapping
    |> List.filter isvalid_2


let puzzle_1 = 
    source
    |> Seq.choose mapping
    |> Seq.filter isvalid_1
    |> Seq.length

let puzzle_2 = 
    source
    |> Seq.choose mapping
    |> Seq.filter isvalid_2
    |> Seq.length


[<Fact>]
let ``have source file`` () = source |> Seq.isEmpty |> should equal false

[<Fact>]
let ``puzzle 1 is correct`` () = puzzle_1 |> should equal 542

[<Fact>]
let ``puzzle 2 is correct`` () = puzzle_2 |> should equal 360
