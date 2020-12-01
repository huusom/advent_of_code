#load "../utils.fsx"

open System.IO

let year = __SOURCE_DIRECTORY__ |> utils.split '\\' |> Option.map Seq.last 
__SOURCE_FILE__ 






let source = utils.load "2015" "01" |> Seq.head

source 
    |> Seq.map (function '(' -> 1 | _ -> -1)
    |> Seq.sum

source 
    |> Seq.map (function '(' -> 1 | _ -> -1)
    |> Seq.scan (+) 0 
    |> Seq.takeWhile ((<>) (-1))
    |> Seq.length