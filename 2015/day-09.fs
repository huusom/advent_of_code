module aoc2015.day_09

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

open FsUnit.Xunit
open Xunit
open Strings

let source = File.load 09

let parse =
    function
    | Rx "(\w+) to (\w+) = (\d+)" [ f; t; d ] -> ((f, t), (int d))

let find routes (f, t) =
    let v = Map.tryFind (f, t) routes
    if Option.isSome v then v else Map.tryFind (t, f) routes

let add a b =
    match (a, b) with
    | None, _
    | _, None -> None
    | (Some a'), (Some b') -> Some(a' + b')

let cost routes path =
    match path
          |> Seq.pairwise
          |> Seq.map (find routes)
          |> Seq.reduce add with
    | None -> None
    | Some c -> Some(path, c)

let routes = source |> Seq.map parse |> Map.ofSeq

let planets =
    seq {
        for ((a, b), _) in (Map.toSeq routes) do
            yield a
            yield b
    }
    |> Seq.distinct
    |> Seq.toList

let paths =
    planets
    |> Sets.perms
    |> Seq.choose (cost routes)
    |> Seq.cache

[<Fact>]
let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false

[<Fact>]
let ``puzzle 1 is correct`` () =
    paths |> Seq.minBy snd |> should equal 141

[<Fact>]
let ``puzzle 2 is correct`` () =
    paths |> Seq.maxBy snd |> should equal 736
