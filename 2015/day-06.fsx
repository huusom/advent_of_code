#load "../utils.fsx"

open System
open System.Text.RegularExpressions

let source = utils.load "2015" "06" 


type Instruction = 
    | Off 
    | On 
    | Toggle 


let tokenize line = 
    Regex.Match(line, @"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)").Groups 
    |> Seq.cast<Group> 
    |> Seq.skip 1
    |> Seq.map (fun g -> g.Value) 
    |> Seq.toList

let parse =
    function
    | "turn off"::a::b::c::d::_ -> ( int a, int b, int c, int d , Off )
    | "turn on"::a::b::c::d::_ -> ( int a, int b, int c, int d , On )
    | "toggle"::a::b::c::d::_ -> ( int a, int b, int c, int d  , Toggle )

let count (array : int [,]) = 
    seq { for w in 0..(array.GetLength(0)-1) do array.[w, *] |> Seq.sum } |> Seq.sum

// puzzle 1
let scanner_1 array (x,y,w,h,i) = 
    for x' in x..w do   
        for y' in y..h do 
            let c = Array2D.get array x' y' 
            match i with
            | Off -> array.[x',y'] <- 0
            | On -> array.[x',y'] <- 1
            | Toggle -> array.[x',y'] <- if (c = 0) then 1 else 0
    array

source 
    |> Seq.map (tokenize >> parse)
    |> Seq.fold scanner_1 (Array2D.create 1000 1000 0) 
    |> count

// puzzle 2
let scanner_2 array (x,y,w,h,i) = 
    for x' in x..w do   
        for y' in y..h do 
            let c = Array2D.get array x' y' 
            match i with
            | Off -> array.[x',y'] <- max 0 (c - 1)
            | On -> array.[x',y'] <- c + 1
            | Toggle -> array.[x',y'] <- c + 2
    array

source 
    |> Seq.map (tokenize >> parse)
    |> Seq.fold scanner_2 (Array2D.create 1000 1000 0) 
    |> count
