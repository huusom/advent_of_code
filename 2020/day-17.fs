module aoc2020.day_17

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

open FsUnit.Xunit
open Xunit

let source = File.load 17

let puzzle_1 = 0

let puzzle_2 = 0

module Cube =
    let parse size input =
        input
        |> Seq.concat
        |> Seq.mapi (fun i c -> c, (i % size, i / size, 0))
        |> Seq.filter (fst >> (=) '#')
        |> Seq.map snd
        |> Set.ofSeq

    let range a b = [ (a - 1) .. (b + 1) ]

    let neighbors (x, y, z) cube =
        seq {
            for x' in range x x do
                for y' in range y y do
                    for z' in range z z do
                        if Set.contains (x', y', z') cube
                           && (x, y, z) <> (x', y', z') then
                            yield 1
        }
        |> Seq.sum

    let fold f (x, y, z) (x', y', z') = (f x x'), (f y y'), (f z z')
    let lower cube = Seq.reduce (fold min) cube
    let upper cube = Seq.reduce (fold max) cube

    let cycle cube =
        let x', y', z' = lower cube
        let x'', y'', z'' = upper cube

        seq {
            for x in range x' x'' do
                for y in range y' y'' do
                    for z in range z' z'' do
                        let p = (x, y, z)

                        let f =
                            match (Set.contains p cube), (neighbors p cube) with
                            | true, 2
                            | true, 3
                            | false, 3 -> true
                            | _ -> false

                        if f then p

        }
        |> Set.ofSeq

    let print cube =
        let x', y', z' = lower cube
        let x'', y'', z'' = upper cube

        for z in z' .. z'' do
            printfn "z=%i" z

            for y in y' .. y'' do
                for x in x' .. x'' do
                    let p = (x, y, z)

                    if (Set.contains p cube) then
                        printf "#"
                    else
                        printf "."

                printfn ""

            printfn ""

        cube

module Hyper =
    let parse size input =
        input
        |> Seq.concat
        |> Seq.mapi (fun i c -> c, (i % size, i / size, 0, 0))
        |> Seq.filter (fst >> (=) '#')
        |> Seq.map snd
        |> Set.ofSeq

    let range a b = [ (a - 1) .. (b + 1) ]

    let neighbors (x, y, z, w) cube =
        seq {
            for x' in range x x do
                for y' in range y y do
                    for z' in range z z do
                        for w' in range w w do
                            if Set.contains (x', y', z', w') cube
                               && (x, y, z, w) <> (x', y', z', w') then
                                yield 1
        }
        |> Seq.sum

    let fold f (x, y, z, w) (x', y', z', w') = (f x x'), (f y y'), (f z z'), (f w w')
    let lower cube = Seq.reduce (fold min) cube
    let upper cube = Seq.reduce (fold max) cube

    let cycle cube =
        let x', y', z', w' = lower cube
        let x'', y'', z'', w'' = upper cube

        seq {
            for x in range x' x'' do
                for y in range y' y'' do
                    for z in range z' z'' do
                        for w in range w' w'' do
                            let p = (x, y, z, w)

                            let f =
                                match (Set.contains p cube), (neighbors p cube) with
                                | true, 2
                                | true, 3
                                | false, 3 -> true
                                | _ -> false

                            if f then p

        }
        |> Set.ofSeq




let ``example is correct`` () =
    let cube = Cube.parse 3 [ ".#."; "..#"; "###" ]

    [ 1 .. 6 ]
    |> List.fold (fun c _ -> Cube.cycle c) cube
    |> Set.count
    |> should equal 112

    let hyper = Hyper.parse 3 [ ".#."; "..#"; "###" ]

    [ 1 .. 6 ]
    |> List.fold (fun c _ -> Hyper.cycle c) hyper
    |> Set.count
    |> should equal 848



let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false


let ``puzzle 1 is correct`` () =
    let cube = Cube.parse 8 source

    [ 1 .. 6 ]
    |> List.fold (fun c _ -> Cube.cycle c) cube
    |> Set.count
    |> should equal 276


let ``puzzle 2 is correct`` () = 
    let cube = Hyper.parse 8 source

    [ 1 .. 6 ]
    |> List.fold (fun c _ -> Hyper.cycle c) cube
    |> Set.count
    |> should equal 276

