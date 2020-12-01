module Puzzles.day_04

open FsUnit.Xunit
open Xunit
open Lib

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 04 


[<Fact>]
let ``have source file`` () =  source |> should not' (be None)

let explode (n : int) =
    n
    |> (string)
    |> Seq.map (int)
    |> Seq.pairwise

let lt (a, b) = a <= b
let eq (a, b) = a = b

let first number =
    let p = number |> explode
    let x = p |> Seq.forall lt
    let y = p |> Seq.exists eq
    x && y

let second number =
    let p = number |> explode
    let x = p |> Seq.forall lt
    let y = p |> Seq.filter eq |> Seq.groupBy id |> Seq.map (snd >> Seq.length) |> Seq.exists ((=) 1)
    x && y

[<Theory>]
[<InlineData(111111, true)>]
[<InlineData(223450, false)>]
[<InlineData(123789, false)>]
let testFirst(number, expected) =
    let actual = first number
    Assert.Equal(expected, actual)

[<Theory>]
[<InlineData(112233, true)>]
[<InlineData(123444, false)>]
[<InlineData(111122, true)>]
let testSecond(number, expected) =
    let actual = second number
    Assert.Equal(expected, actual)

[<Theory>]
[<InlineData(931, 609)>]
let validate(expectedFirst, expectedSecond) =
    let actualFirst = [272091 .. 815432] |> Seq.filter first |> Seq.length
    Assert.Equal(expectedFirst, actualFirst)

    let actualSecond = [272091 .. 815432] |> Seq.filter second |> Seq.length
    Assert.Equal(expectedSecond, actualSecond)
