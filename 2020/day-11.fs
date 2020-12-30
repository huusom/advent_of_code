module aoc2015.day_11

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

open FsUnit.Xunit
open Xunit

let source = File.load 11

let data =
    [ "L.LL.LL.LL"
      "LLLLLLL.LL"
      "L.L.L..L.."
      "LLLL.LL.LL"
      "L.LL.LL.LL"
      "L.LLLLL.LL"
      "..L.L....."
      "LLLLLLLLLL"
      "L.LLLLLL.L"
      "L.LLLLL.LL" ]
    |> List.map (Seq.toList)
    |> array2D


let print input =
    let l = Array2D.length1 input - 1

    for i in 0 .. l do
        input.[i, *] |> System.String |> printfn "%s"

let occupied =
    function
    | '#' -> 1
    | _ -> 0


let seat current x neighboors =
    let n = neighboors - (occupied current)

    match current with
    | 'L' when n = 0 -> '#'
    | '#' when n >= x -> 'L'
    | _ -> current

let rule_1 input i j c =
    let w = Array2D.length1 input - 1 |> min (i + 1)
    let h = Array2D.length2 input - 1 |> min (j + 1)
    let x = max 0 (i - 1)
    let y = max 0 (j - 1)

    seq {
        for x' in x .. w do
            for y' in y .. h do
                yield input.[x', y'] |> occupied
    }
    |> Seq.sum
    |> seat c 4


let spiral =
    seq {
        for x in -1 .. 1 do
            for y in -1 .. 1 do
                if (x, y) <> (0, 0) then yield (x, y)
    }
    |> Seq.cache

let nearest input (x, y) (dx, dy) =
    try
        seq {
            for i in 1 .. 100 do
                let c =
                    Array2D.get input (x + dx * i) (y + dy * i)

                if c <> '.' then yield c
        }
        |> Seq.head
    with _ -> '.'

let rule_2 input i j c =
    spiral
    |> Seq.map (nearest input (i, j))
    |> Seq.sumBy occupied
    |> (+) (occupied c)
    |> seat c 5

let rec find rules input =
    let next = Array2D.mapi (rules input) input

    if next <> input then
        find rules next
    else
        next

let count input =
    let w = Array2D.length1 input - 1
    let h = Array2D.length2 input - 1

    seq {
        for x in 0 .. w do
            for y in 0 .. h do
                occupied input.[x, y]
    }
    |> Seq.sum

[<Fact>]
let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false

[<Fact>]
let ``puzzle 1 is correct`` () =
    source
    |> array2D
    |> find rule_1
    |> count
    |> should equal 2243

let ``puzzle 2 is correct`` () =
    source
    |> array2D
    |> find rule_2
    |> count
    |> should equal 2027

