module Puzzles.day_13

open FsUnit.Xunit
open Xunit
open Lib

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 13 

let puzzle_1 = 0

let puzzle_2 = 0


[<Fact(Skip = "Not done yet")>]
let ``have source file`` () =  source |> should not' (be None)

[<Fact(Skip = "Not done yet")>]
let ``puzzle 1 is correct`` () = puzzle_1 |> should equal 0

[<Fact(Skip = "Not done yet")>]
let ``puzzle 2 is correct`` () = puzzle_2 |> should equal 0
