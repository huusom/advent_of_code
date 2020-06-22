module Y2019.D08

open System.IO
open Library

let ``solve 1`` input =
    let layers = input |> Seq.chunkBySize (25 * 6)

    let count ch layer =
        layer
        |> Seq.filter (fun i -> i = ch)
        |> Seq.length

    layers
    |> Seq.minBy (count '0')
    |> fun l -> (count '2' l) * (count '1' l)


let ``solve 2`` w h input =
    let layers = input |> Seq.chunkBySize (w * h)

    let colors =
        seq {
            for i in 0 .. (w * h - 1) do
                layers
                |> Seq.map (Seq.item i)
                |> Seq.find ((<>) '2')
        }

    colors
    |> Seq.map (function
        | '0' -> ' '
        | _ -> 'X')
    |> Seq.splitInto 6
    |> Seq.map (fun x -> System.String(x))
    |> String.concat "\n"


let input = File.ReadAllText "input/D08.txt"

[<Xunit.Fact>]
let ``result 1``() = ``solve 1`` input =! 2159

[<Xunit.Fact>]
let ``result 2``() = ``solve 2`` 25 6 input =! @" XX    XX XXXX X  X XXX
X  X    X    X X  X X  X 
X       X   X  XXXX X  X 
X       X  X   X  X XXX  
X  X X  X X    X  X X X  
 XX   XX  XXXX X  X X  X "
