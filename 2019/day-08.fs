module aoc2019.day08

open FsUnit.Xunit
open Xunit
open Lib

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let w = 25
let h = 6
let source = File.text 08 |> Seq.chunkBySize (w * h)

let count ch = Seq.sumBy (function | x when x = ch -> 1 | _ -> 0)

let colors =
    seq {
        for i in 0 .. (w * h - 1) do
            source
            |> Seq.map (Seq.item i)
            |> Seq.find ((<>) '2')
    }

[<Fact>]
let ``result 1``() =
    source
    |> Seq.minBy (count '0')
    |> fun l -> (count '2' l) * (count '1' l)
    |> should equal 2159

[<Fact>]
let ``result 2``() = 
    colors
    |> Seq.map (function
        | '0' -> ' '
        | _ -> 'X')
    |> Seq.splitInto 6
    |> Seq.map (fun x -> System.String(x))
    |> Seq.toList
    |> should equal [" XX    XX XXXX X  X XXX  "; "X  X    X    X X  X X  X ";"X       X   X  XXXX X  X ";"X       X  X   X  X XXX  ";"X  X X  X X    X  X X X  ";" XX   XX  XXXX X  X X  X "]
