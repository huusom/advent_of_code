module Puzzles.day_18

open FsUnit.Xunit
open Xunit
open Lib

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 18 

let puzzle_1 = 0

let puzzle_2 = 0



let ``have source file`` () =  source |> Seq.isEmpty |> should equal false


let ``puzzle 1 is correct`` () = puzzle_1 |> should equal 0


let ``puzzle 2 is correct`` () = puzzle_2 |> should equal 0
