module aoc2015.day_05

open FsUnit.Xunit
open Xunit

open System.Text.RegularExpressions

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 05

let is_nice_1 line =
    let has_three_vowels =
        Regex.Matches(line, "[aeiou]").Count >= 3

    let douple_letters = Regex.IsMatch(line, "(\w)\1")

    let not_contains =
        Regex.IsMatch(line, "(ab|cd|pq|xy)") |> not

    has_three_vowels && douple_letters && not_contains

let is_nice_2 line =
    let x = Regex.IsMatch(line, @"(\w\w)\w*\1")
    let y = Regex.IsMatch(line, @"(\w)\w\1")
    x && y


[<Fact>]
let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false


[<Fact>]
let ``puzzle 1 is correct`` () =
    source
    |> Seq.filter is_nice_1
    |> Seq.length
    |> should equal 236


[<Fact>]
let ``puzzle 2 is correct`` () =
    source
    |> Seq.filter is_nice_2
    |> Seq.length
    |> should equal 51
