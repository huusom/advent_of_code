module aoc2020.day_09

open FsUnit.Xunit
open Xunit

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 09

let pairs line =
    let l = Array.length line - 1
    let t = Array.get line l

    seq {
        for a in 0 .. l do
            for b in (a + 1) .. l do
                let a' = line.[a]
                let b' = line.[b]
                if a' + b' = t then yield true
    } 
    |> Seq.exists id

let find size input =
    input
    |> Seq.map int
    |> Seq.windowed (size + 1)
    |> Seq.filter (pairs >> not)
    |> Seq.head
    |> Seq.last

exception Found of int list

let rec folder t current next =
    match List.sum current with
    | 0 -> [ next ]
    | i when i < t -> List.append current [ next ]
    | i when i > t -> folder t (List.tail current) next
    | t -> raise (Found current)

let search target input =
    try
        input
        |> Seq.map int
        |> Seq.fold (folder target) []
        |> List.sum
    with Found values ->
        let l = values |> List.sort
        (List.head l) + (List.last l)

[<Fact>]
let ``have source file``  =
    source |> Seq.isEmpty |> should equal false

[<Fact>]
let ``puzzle 1 is correct``  =
    source |> find 25 |> should equal 14360655

[<Fact>]
let ``puzzle 2 is correct``  =
    source |> search 14360655 |> should equal 1962331

