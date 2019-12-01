open System.IO

let calculate_1 i = i / 3 - 2

let rec calculate_2 i =
    let chooser x = if x <= 0 then None else Some (x, x)
    Seq.unfold (calculate_1 >> chooser) i
    |> Seq.sum

let eval f = 
    Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__.Replace("fsx", "txt"))
    |> File.ReadLines
    |> Seq.map (int)
    |> Seq.map f
    |> Seq.sum

let answer_1 = eval calculate_1
let answer_2 = eval calculate_2
