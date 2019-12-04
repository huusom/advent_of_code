#load "references.fsx"

open AdventOfCode
open System.Text.RegularExpressions

let toPairs n =
    n
    |> Seq.map (fun c -> (int c) - (int '0'))
    |> Seq.pairwise

let isIncreasing (a, b) = a <= b
let isEqual (a, b) = a = b

let isValid i =
    let n =  sprintf "%i" i
    let p = toPairs n
    let x = p |> Seq.forall isIncreasing
    let y = p |> Seq.exists isEqual
    x && y

let a =
    seq {
        for n in 272091 .. 815432 do
            isValid n
    }
    |> Seq.filter id
    |> Seq.length


 tokenize (Regex @"(\d)\1") "111122"

