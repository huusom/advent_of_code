module aoc2015.day_08

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

open FsUnit.Xunit
open Xunit
open System.Text.RegularExpressions

let source = File.load 08

let rx = Regex @"\\x[a-f0-9][a-f0-9]|\\""|\\\\" 

let replace line = rx.Replace(line, "_")

let count_1 line = 
    let l2 = replace line |> String.length
    l2 - 2

let count_2 (line:string) =
    line
        .Replace("\\", "\\\\")
        .Replace("\"", "\\\"")
    |> String.length
    |> (+) 2


[<Fact>]
let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false

[<Fact>]
let ``puzzle 1 is correct`` () = 
    let l1 = source |> Seq.sumBy String.length
    let l2 = source |> Seq.sumBy count_1
    l1 - l2 |> should equal 1371

[<Fact>]
let ``puzzle 2 is correct`` () =
    let l1 = source |> Seq.sumBy String.length
    let l2 = source |> Seq.sumBy count_2
    l2 - l1 |> should equal 2117

