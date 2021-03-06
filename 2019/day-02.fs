module aoc2019.day_02

open FsUnit.Xunit
open Xunit

open IntCode

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.text 2 

let program a b =
    let p = Program.load source
    p.M.[1] <- a
    p.M.[2] <- b
    p

let eval p = (Program.run p).M.[0]

[<Theory>]
[<InlineData("1,0,0,0,99", "2,0,0,0,99")>]
[<InlineData("2,3,0,3,99", "2,3,0,6,99")>]
[<InlineData("2,4,4,5,99,0", "2,4,4,5,99,9801")>]
[<InlineData("1,1,1,4,99,5,6,0,99", "30,1,1,4,2,5,6,0,99")>]
let test (source, expected) =
    let p =
        source
        |> Program.load
        |> Program.run

    p.M
    |> Seq.map (string)
    |> String.concat ","
    |> should equal expected

[<Theory>]
[<InlineData(3706713)>]
let first (expected) =
    let p = program 12 2

    let actual = eval p
    Assert.Equal(expected, actual)

[<Theory>]
[<InlineData(8609)>]
let second (expected) =
    let a, b =
        seq {
            for a in 0 .. 99 do
                for b in 0 .. 99 do
                    (program a b |> eval), (a, b)
        }
        |> Seq.find (fst >> (=) 19690720)
        |> snd
    Assert.Equal(expected, 100 * a + b)