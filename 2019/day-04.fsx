#load "references.fsx"

open AdventOfCode
open System.Text.RegularExpressions

let toPairs n =
    n
    |> Seq.map (fun c -> (int c) - (int '0'))
    |> Seq.pairwise

let isIncreasing (a, b) = a <= b
let isEqual (a, b) = a = b


let rule1 i =
    let n = sprintf "%i" i
    let p = toPairs n
    let x = p |> Seq.forall isIncreasing
    let y = p |> Seq.exists isEqual
    x && y

let rule2 i =
    sprintf "%i" i
    |> toPairs
    |> Seq.filter isEqual
    |> Seq.groupBy id   
    |> Seq.map (snd >> Seq.length)
    |> Seq.exists ((=) 1)

let a =
    [ 272091 .. 815432 ]
    |> Seq.filter rule1
    |> Seq.filter rule2
    |> Seq.length

