module Strings

open System.Text.RegularExpressions

let split (c: char) (s: string) = s.Split([| c |])

let substrings i (s: string) = (s.Substring(0, i)), (s.Substring(i))

let replace (a: string) b (s: string) = s.Replace(a, b)

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

let (|Rx|_|) pattern input =
    let m = Regex(pattern).Match(input)

    if m.Success then
        [ for g in m.Groups -> g.Value ]
        |> List.tail
        |> Some
    else
        None

let (/) s i = substrings i s
let (=~) i p = Regex.IsMatch(i, p)