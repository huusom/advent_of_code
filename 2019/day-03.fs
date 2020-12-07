module aoc2019.day_03

open FsUnit.Xunit
open Xunit
open Lib
open Lib.Strings
open IntCode


#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif


let steps move =
    match substrings 1 move with
    | ("R", num) ->
        seq {
            for _ in 1 .. (int num) do
                (1, 0)
        }
    | ("D", num) ->
        seq {
            for _ in 1 .. (int num) do
                (0, -1)
        }
    | ("L", num) ->
        seq {
            for _ in 1 .. (int num) do
                (-1, 0)
        }
    | ("U", num) ->
        seq {
            for _ in 1 .. (int num) do
                (0, 1)
        }
    | _ -> Seq.empty


let forward tokens =
    tokens
    |> Seq.collect steps
    |> Seq.scan (fun (a, b) (c, d) -> (a + c, b + d)) (0, 0)
    |> Seq.tail

let tokenize = split ','

let intersect a b =
    let a' = Set.ofSeq a
    let b' = Set.ofSeq b
    Set.intersect a' b'

let first a b =
    intersect a b
    |> Seq.map (fun (x, y) -> (abs x) + (abs y))
    |> Seq.min

let second a b =
    let count c = 2 + (Seq.findIndex ((=) c) a) + (Seq.findIndex ((=) c) b)
    intersect a b
    |> Seq.map count
    |> Seq.min


[<Theory>]
[<InlineData("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83", 159)>]
[<InlineData("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 135)>]
let testFirst (line1, line2, expected) =
    let l1 =
        line1
        |> tokenize
        |> forward

    let l2 =
        line2
        |> tokenize
        |> forward

    Assert.Equal(expected, first l1 l2)

[<Theory>]
[<InlineData("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83", 610)>]
[<InlineData("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7", 410)>]
let testSecond (line1, line2, expected) =
    let l1 =
        line1
        |> tokenize
        |> forward

    let l2 =
        line2
        |> tokenize
        |> forward

    Assert.Equal(expected, second l1 l2)

[<Theory>]
[<InlineData(1626, 27330)>]
let solve (expectedFirst, expectedSecond) =
    let lines = File.load 3 |> Seq.toArray

    let l1 =
        lines.[0]
        |> tokenize
        |> forward

    let l2 =
        lines.[1]
        |> tokenize
        |> forward

    Assert.Equal(expectedFirst, first l1 l2)
    Assert.Equal(expectedSecond, second l1 l2)
