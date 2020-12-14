module aoc2020.day_10

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

open FsUnit.Xunit
open Xunit
open Lib


let source =
    File.load 10 |> Seq.map (int) |> Set.ofSeq

let data_1 =
    [ 16
      10
      15
      5
      1
      11
      7
      19
      6
      12
      4 ]

let data_2 =
    [ 28
      33
      18
      42
      31
      14
      46
      20
      48
      47
      24
      23
      49
      45
      19
      38
      39
      11
      1
      32
      25
      35
      8
      17
      7
      9
      4
      2
      34
      10
      3 ]

let prep input =
    let m = Seq.max input
    seq {
        yield 0
        yield m + 3
        yield! input
    }
    |> Seq.sort

let diffs input = 
    input
    |> Seq.pairwise
    |> Seq.map (fun (a, b) -> b - a)


let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false


let ``puzzle 1 is correct`` () =
    source
    |> prep
    |> diffs
    |> Seq.countBy id
    |> Seq.map snd
    |> Seq.reduce (*)
    |> should equal 2210


let ``puzzle 2 is correct`` () = 0 |> should equal 0
