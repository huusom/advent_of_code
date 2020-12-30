module aoc2020.day_12

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

open Xunit
open FsUnit.Xunit
open Strings


let source = File.load 12


let parse =
    function
    | Rx "(\w)(\d+)" [ d; i ] -> (d, (int i))
    | x -> failwithf "wtf: %s" x

let compas = [ "N"; "E"; "S"; "W" ]

let turn h r (i: int) =
    let s = if r = "R" then 1 else -1
    let c = (List.findIndex ((=) h) compas)
    let nth = (4 + i / 90 * s + c) % 4
    List.item nth compas

let rec move (h, x, y) =
    function
    | "N", i -> h, x + i, y
    | "E", i -> h, x, y + i
    | "S", i -> h, x - i, y
    | "W", i -> h, x, y - i
    | "F", i -> move (h, x, y) (h, i)
    | r, i -> (turn h r i), x, y

let manhattan_distance (x, y) = (abs x) + (abs y)

type Ship =
    { H: string
      C: int * int
      W: int * int }

let ship = { H = "E"; C = 0, 0; W = 10, 1 }

let (++) (a, b) (c, d) = (a + c, b + d)
let (+*) (a, b) c = a * c, b * c

let radians = 0.01745329252

let rotate (x, y) degree =
    let s = sin (float degree * radians)
    let c = cos (float degree * radians)
    let x' = float x
    let y' = float y

    let x'' =
        (x' * c + y' * s) |> System.Math.Round |> int

    let y'' =
        (y' * c - x' * s) |> System.Math.Round |> int

    x'', y''

let navigate ship =
    function
    | "N", i -> { ship with W = ship.W ++ (0, i) }
    | "S", i -> { ship with W = ship.W ++ (0, -i) }
    | "E", i -> { ship with W = ship.W ++ (i, 0) }
    | "W", i -> { ship with W = ship.W ++ (-i, 0) }
    | "F", i ->
        { ship with
              C = ship.C ++ (ship.W +* i) }
    | "R", i -> { ship with W = rotate ship.W i }
    | "L", i -> { ship with W = rotate ship.W -i }


let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false


let ``puzzle 1 is correct`` () =
    source
    |> Seq.map parse
    |> Seq.fold move ("E", 0, 0)
    |> fun (_, x, y) -> manhattan_distance (x, y)
    |> should equal 2280

let ``puzzle 2 is correct`` () =
    source
    |> Seq.map parse
    |> Seq.fold navigate ship
    |> fun s -> manhattan_distance s.C
    |> should equal 38693
// source |> Seq.map parse
