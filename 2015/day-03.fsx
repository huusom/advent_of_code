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

// challenge 1
source
    |> calc
    |> Seq.distinct
    |> Seq.length


// part two

let evenodd i x = (i % 2 = 0), x

let filter f input = 
    input 
    |> Seq.mapi evenodd 
    |> Seq.filter (fun (santa, _) -> santa = f)
    |> Seq.map snd

let santas = 
    source 
    |> filter true
    |> calc

let robots = 
    source 
    |> filter false
    |> calc

Seq.append santas robots 
    |> Seq.distinct
    |> Seq.length
