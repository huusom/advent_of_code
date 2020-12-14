module aoc2020.day_10

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

open FsUnit.Xunit
open Xunit

let source =
    File.load 10 |> Seq.map (int) |> Set.ofSeq

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

let to_string input =
    input |> Seq.map (string) |> String.concat ""

let to_array str =
    System.Text.RegularExpressions.Regex.Split(str, "3+")
    |> Seq.filter (System.String.IsNullOrEmpty >> not)

let to_int64 = 
    function 
    | "1111" -> 7L
    | "111" -> 4L
    | "11" -> 2L
    | _ -> 1L

[<Fact>]
let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false

[<Fact>]
let ``puzzle 1 is correct`` () =
    source
    |> prep
    |> diffs
    |> Seq.countBy id
    |> Seq.map snd
    |> Seq.reduce (*)
    |> should equal 2210

[<Fact>]
let ``puzzle 2 is correct`` () =
    source
    |> prep
    |> diffs
    |> to_string
    |> to_array
    |> Seq.map to_int64
    |> Seq.reduce (*)
    |> should equal 7086739046912L



