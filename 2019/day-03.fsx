#load "references.fsx"

open AdventOfCode

let input =
    combine __SOURCE_DIRECTORY__ __SOURCE_FILE__
    |> replace "fsx" "txt"
    |> load
    |> Option.get

let parse s =
    match substrings 1 s with
    | "R",x -> ((int) x), 0
    | "L",x -> (0-(int) x), 0
    | "U",x -> (0, 0-(int) x)
    | "D",x -> (0, (int) x)

let tokenize line = split ',' line |> Seq.map toint
 
 "R75,D30,R83,U83,L12,D49,R71,U7,L72,U62,R66,U55,R34,D71,R55,D58,R83"
    |> tokenize