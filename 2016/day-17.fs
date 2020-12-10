module aoc2016.day_17

open FsUnit.Xunit
open Xunit


#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 17 

let puzzle_1 = 0

let puzzle_2 = 0



let ``have source file`` () =  source |> should not' (be None)


let ``puzzle 1 is correct`` () = puzzle_1 |> should equal 0


let ``puzzle 2 is correct`` () = puzzle_2 |> should equal 0
