module AdventOfCode

open System.IO
open System.Text.RegularExpressions

let replace (a: string) b (s: string) = s.Replace(a, b)

let combine dir file = Path.Combine(dir, file)

let load path =
    if not (File.Exists path) then
        None
    else
        File.ReadLines path
        |> Seq.cast<string>
        |> Some

let tokenize (rx: Regex) line =
    let m = rx.Match line
    if not m.Success then
        None
    else
        m.Groups
        |> Seq.cast<Group>
        |> Seq.skip 1
        |> Seq.map (fun g -> g.Value)
        |> Seq.toArray
        |> Some

let loadMap mapper path = load path |> Option.map (Seq.map mapper)

let loadChoose chooser path = load path |> Option.map (Seq.choose chooser)

let loadTokens regex = loadChoose (tokenize (Regex regex))
