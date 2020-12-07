module aoc2015.day_01

open Xunit
open FsUnit.Xunit
open Lib

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 1 |> Seq.head

let puzzle_1 =
    source
    |> Seq.map (function
        | '(' -> 1
        | _ -> -1)
    |> Seq.sum

let puzzle_2 =
    source
    |> Seq.map (function
        | '(' -> 1
        | _ -> -1)
    |> Seq.scan (+) 0
    |> Seq.takeWhile ((<>) (-1))
    |> Seq.length


[<Fact>]
[<Trait("Category", "2015")>]
let ``have source file`` () =  source |> should not' (equal "")

[<Fact>]
let ``puzzle 1 is correct`` () = puzzle_1 |> should equal 74

[<Fact>]
let ``puzzle 2 is correct`` () = puzzle_2 |> should equal 1795
