module Puzzles.day_05

open FsUnit.Xunit
open Xunit
open Lib

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 05

let puzzle_1 = 0

let puzzle_2 = 0

let upper (x, y) =
    let h = (y - x + 1) / 2
    (x + h, y)

let lower (x, y) =
    let h = (y - x) / 2
    (x, x + h)

let divide (r, c) =
    function
    | 'F' -> (lower r), c
    | 'B' -> (upper r), c
    | 'R' -> r, (upper c)
    | _ -> r, (lower c)

let seat_id line =
    Seq.scan divide ((0, 127), (0, 7)) line
    |> Seq.toList
    |> Seq.last
    |> fun x -> (x |> fst |> fst) * 8 + (x |> snd |> snd)


[<Fact>]
let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false

[<Fact>]
let ``puzzle 1 is correct`` () =
    source
    |> Seq.map seat_id
    |> Seq.max
    |> should equal 0

[<Fact>]
let ``puzzle 2 is correct`` () =
    let seats =
        source |> Seq.map seat_id |> Seq.sort |> Set.ofSeq

    [ 0 .. (seats.Count) ]
    |> List.filter (fun i -> Set.contains i seats |> not)
    |> List.last
    |> should equal 636
