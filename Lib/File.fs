module File

open System
open System.IO

let load (day: int) =
    let path = String.Format(@"inputs\{0:00}.txt", day)
    if File.Exists path 
    then
        path
        |> File.ReadAllLines
        |> Seq.cache
    else    
        Seq.empty

let text (day: int) = 
    let path = String.Format(@"inputs\{0:00}.txt", day)
    if File.Exists path 
    then
        path
        |> File.ReadAllText
    else    
        String.Empty
