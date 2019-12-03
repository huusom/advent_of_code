#load "references.fsx"

open AdventOfCode

let parse s =
    match substrings 1 s with
    | "R",x -> ((int) x), 0
    | "L",x -> (0-(int) x), 0
    | "U",x -> (0, (int) x)
    | "D",x -> (0, 0-(int) x)

let tokenize line = split ',' line |> Seq.map parse
 
let move ((x, y), t) =
    match t with
    | (0, y') -> seq { for dy in 0 .. (sign y') .. y' do (x, y+dy) }
    | (x', 0) -> seq { for dx in 0 .. (sign x') .. x' do (x+dx, y)}

let scanner ((a, b), (c,d)) x = (a+c, b+d), x

let steps sq = 
    let h = Seq.head sq
    let t = Seq.tail sq
    Seq.scan scanner ((0,0), h) t
    |> Seq.map move    
    |> Seq.map (Seq.skip 1)
    |> Seq.concat

let intersect a b = 
    let a' = Set.ofSeq a
    let b' = Set.ofSeq b
    Set.intersect a' b'

let manhattan_dist a b = 
    intersect a b
    |> Seq.map (fun (x, y) -> (abs x)  + (abs y))
    |> Seq.min

let count_steps a b =
    let count c = 
        2 + (Seq.findIndex ((=) c) a)  + (Seq.findIndex ((=) c) b)
    intersect a b
        |> Seq.map count
        |> Seq.min    

let input =
    combine __SOURCE_DIRECTORY__ __SOURCE_FILE__
    |> replace "fsx" "txt"
    |> load
    |> Option.get

let line_1 = input |> Seq.item 0 |> tokenize |> steps
let line_2 = input |> Seq.item 1 |> tokenize |> steps

manhattan_dist line_1 line_2
count_steps line_1 line_2

