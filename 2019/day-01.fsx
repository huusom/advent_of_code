#load "references.fsx"

open AdventOfCode

let input =
    combine __SOURCE_DIRECTORY__ __SOURCE_FILE__
    |> replace "fsx" "txt"
    |> loadMap (int)

let eval f = Option.map f input

let calculate_1 i = i / 3 - 2

let rec calculate_2 i =
    let chooser x =
        if x <= 0 then None
        else Some(x, x)
    Seq.unfold (calculate_1 >> chooser) i |> Seq.sum

let answer_1 = eval (Seq.sumBy calculate_1)
let answer_2 = eval (Seq.sumBy calculate_2)
