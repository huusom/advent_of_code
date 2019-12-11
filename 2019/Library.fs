﻿module Library

open System.IO
open System.Text.RegularExpressions

let split (c:char) (s:string) = s.Split([|c|])

let substrings i (s:string) = (s.Substring(0, i)),(s.Substring(i))
 
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

let distrib e L =
    let rec aux pre post = 
        seq {
            match post with
            | [] -> yield (L @ [e])
            | h::t -> yield (List.rev pre @ [e] @ post)
                      yield! aux (h::pre) t 
        }
    aux [] L

let rec perms = function 
    | [] -> Seq.singleton []
    | h::t -> Seq.collect (distrib h) (perms t)