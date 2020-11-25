module utils

open System 
open System.IO

let load year day = 
    Path.Combine(year, day + ".txt")
    |> File.ReadLines
    |> Seq.cache


let split (delimiter:char) (line : string) = line.Split([| delimiter |], StringSplitOptions.RemoveEmptyEntries) |> function | [| |] -> None | a -> Some a
let replace a (b:string) (line:string) = line.Replace(a, b)
let startsWith (prefix:string) (line:string) = line.StartsWith (prefix) 


