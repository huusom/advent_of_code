#load "../utils.fsx"

open System
open System.IO

let source = utils.load "2015" "02" 

let split (exp : string) = exp.Split('x') |> Seq.map (int) 

let calc values = values |> Seq.toList |> function | [l;w;h] -> [ l * w; w * h;  h * l ] |> fun l -> (List.sum l) * 2 + (List.min l)

let ribbon m =  
    let a = m |> Seq.sort |> Seq.take 2 |> Seq.map (fun x -> x + x) |> Seq.sum
    let b = m |> Seq.fold (*) 1
    a + b

source
    |> Seq.map (split >> calc)
    |> Seq.sum

source
    |> Seq.map (split >> ribbon)
    |> Seq.sum

