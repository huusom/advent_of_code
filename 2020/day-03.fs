module aoc2020.day_03

open FsUnit.Xunit
open Xunit
open Lib

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 03 |> Seq.toArray

let data =
    [| "..##......."
       "#...#...#.."
       ".#....#..#."
       "..#.#...#.#"
       ".#...##..#."
       "..#.##....."
       ".#.#.#....#"
       ".#........#"
       "#.##...#..."
       "#...##....#"
       ".#..#...#.#" |]

let steps input (right, down) =
    let width = input |> Seq.head |> String.length
    let length = input |> Seq.length

    seq { for i in 0 .. ((length - 1) / down) -> (i * right % width), (i * down) }

let lookup (input: string array) (x, y) = input.[y].[x]


let calc input (right, down) =
    steps input (right, down)
    |> Seq.map (lookup input)
    |> Seq.filter ((=) '#')
    |> Seq.length

let puzzle_1 source = calc source (3, 1)

let puzzle_2 source =
    [ 1, 1; 3, 1; 5, 1; 7, 1; 1, 2 ]
    |> List.map (calc source)
    |> List.map (int64)
    |> List.fold (*) 1L

[<Fact>]
let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false

[<Fact>]
let ``puzzle 1 is correct`` () = puzzle_1 source |> should equal 223

[<Fact>]
let ``puzzle 2 is correct`` () = puzzle_2 source |> should equal 3517401300L
