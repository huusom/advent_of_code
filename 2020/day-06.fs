module aoc2020.day06

open FsUnit.Xunit
open Xunit
open Lib

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 06

let folder groups line =
    match groups with
    | _ when line = "" -> Set.empty :: groups
    | group :: rest -> (line |> Set.ofSeq |> Set.union group) :: rest
    | [] -> Set.empty :: []


let add groups line =
    match groups with
    | _ when line = "" -> [] :: groups
    | group :: rest -> (line :: group) :: rest
    | [] -> []

let count group =
    let l = List.length group
    group
    |> String.concat ""
    |> Seq.countBy id
    |> Seq.filter (fun (_, x) -> x = l)
    |> Seq.length


[<Fact>]
let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false

[<Fact>]
let ``puzzle 1 is correct`` () =
    source
    |> Seq.fold folder [ Set.empty ]
    |> Seq.map (Set.count)
    |> Seq.sum
    |> should equal 6782

[<Fact>]
let ``puzzle 2 is correct`` () =
    source
    |> Seq.fold add [ [] ]
    |> Seq.map count
    |> Seq.sum
    |> should equal 3596
