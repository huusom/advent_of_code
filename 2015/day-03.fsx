#load "../utils.fsx"

open System.IO

let source = utils.load "2015" "03" |> Seq.head

let move (x, y) =
    function
    | '^' -> (x, y+1)
    | '>' -> (x+1, y)
    | '<' -> (x-1, y)
    | 'v' -> (x, y-1)
    

let calc input = input |> Seq.scan move (0,0) 


source
    |> calc
    |> Seq.skip 1
    |> Seq.groupBy id
    |> Seq.map (fun (a, b) -> a, (Seq.length b))
    |> Seq.map snd
    |> Seq.sum
    