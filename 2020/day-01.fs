module aoc2020.day_01

open Xunit
open FsUnit.Xunit
open Lib

#if INTERACTIVE
#load "..\Lib\Helpers.fs"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 1  |> Seq.filter (System.String.IsNullOrEmpty >> not) |> Seq.map (int)


let puzzle_1 = 
    let s1 = source |> Set.ofSeq
    let s2 = source |> Seq.map (fun i -> 2020 - i) |> Set.ofSeq
    Set.intersect s1 s2
    |> Seq.reduce (*)

let puzzle_2 = 
    source
    |> Seq.allPairs source
    |> Seq.allPairs source
    |> Seq.find (fun (a, (b, c)) -> (a + b + c) = 2020)
    |> fun (a, (b, c)) -> a * b * c


[<Fact>]
let ``have source file`` () =  source |> Seq.isEmpty |> should equal false

[<Fact>]
let ``puzzle 1 is correct`` () = puzzle_1 |> should equal 605364

[<Fact>]
let ``puzzle 2 is correct`` () = puzzle_2 |> should equal 128397680
