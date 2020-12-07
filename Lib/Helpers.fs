namespace Lib

module File = 

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


module Strings = 
    open System.Text.RegularExpressions

    let split (c:char) (s:string) = s.Split([|c|])
    
    let substrings i (s:string) = (s.Substring(0, i)),(s.Substring(i))
    
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

    let (/) s i = substrings i s 
    let (=~) i p = Regex.IsMatch(i, p)

module Sets = 
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

    let rec gcd =
        function
        | (a, 0) -> abs a
        | (a, b) -> gcd (b, (a % b))

    let lcm a b = (a * b) / gcd (a, b)
