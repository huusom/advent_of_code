open System.IO

Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__.Replace("fsx", "txt"))
    |> File.ReadLines
    |> Seq.map (int)
    |> Seq.map (fun i -> (i / 3) - 2)
    |> Seq.sum
