module Y2019.D08

open System.IO

let input = File.ReadAllText "input/D08.txt"

let layers = input |> Seq.chunkBySize (25 * 6)

let first =
    let count ch layer =
        layer
        |> Seq.filter (fun i -> i = ch)
        |> Seq.length

    layers
    |> Seq.minBy (count '0')
    |> fun l -> (count '2' l) * (count '1' l)


let second input w h = 
    let layers = input |> Seq.chunkBySize (w * h)

    let colors =
        seq {
            for i in 0 .. (w*h-1) do
                layers
                |> Seq.map (Seq.item i)
                |> Seq.find ((<>) '2')
        }

    let print i c =
        if c = '0' then printf " " else printf "X"
        if ((i+1) % w) = 0 then printfn ""

    Seq.iteri print colors

second "0222112222120000" 2 2
second input 25 6
