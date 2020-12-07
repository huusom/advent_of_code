module aoc2015.day_03

open FsUnit.Xunit
open Xunit
open Lib

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.text  3 

let move (x, y) =
    function
    | '^' -> (x, y+1)
    | '>' -> (x+1, y)
    | '<' -> (x-1, y)
    | 'v' -> (x, y-1)
    

let calc input = input |> Seq.scan move (0,0) 

let evenodd i x = (i % 2 = 0), x

let filter f input = 
    input 
    |> Seq.mapi evenodd 
    |> Seq.filter (fun (santa, _) -> santa = f)
    |> Seq.map snd



[<Fact>]
let ``have source file`` () =  source |> Seq.isEmpty |> should equal false


let ``puzzle 1 is correct`` () = 
    source
    |> calc
    |> Seq.distinct
    |> Seq.length
    |> should equal 2081


let ``puzzle 2 is correct`` () = 
    let santas = 
        source 
        |> filter true
        |> calc

    let robots = 
        source 
        |> filter false
        |> calc

    Seq.append santas robots 
        |> Seq.distinct
        |> Seq.length
        |> should equal 2341
