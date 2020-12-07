module aoc2020.day_07

open FsUnit.Xunit
open Xunit
open Lib
open System.Text.RegularExpressions
open Strings

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 07

let rx = Regex @"(\d+) (\w+ \w+) bags?"

let create_rule line =
    let name =
        line
        / (line.IndexOf(' ', line.IndexOf(' ') + 1))
        |> fst

    let system =
        rx.Matches(line)
        |> Seq.map (fun m -> m.Groups.[2].Value, (int m.Groups.[1].Value))
        |> Map.ofSeq

    name, system


let lookup rules name =
    rules
    |> Seq.filter (snd >> Map.containsKey name)
    |> Seq.map fst

let rec find rules name =
    let names = lookup rules name
    names
    |> Seq.map (find rules)
    |> Seq.concat
    |> Seq.append names
    |> Seq.distinct

let rec calc (rules: Map<string, Map<string, int>>) (name, value) =
    match Map.tryFind name rules with
    | None -> [ value ]
    | Some children when children = Map.empty -> [ value ]
    | Some children ->
        let tl =
            children
            |> Map.toSeq
            |> Seq.map (calc rules)
            |> Seq.concat
            |> Seq.map ((*) value)
            |> Seq.toList

        value :: tl


[<Fact>]
let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false

[<Fact>]
let ``puzzle 1 is correct`` () =
    let rules = source |> Seq.map create_rule
    find rules "shiny gold"
    |> Seq.length
    |> should equal 197

[<Fact>]
let ``puzzle 2 is correct`` () = 
    let rules = source |> Seq.map create_rule |> Map.ofSeq
    calc rules ("shiny gold", 1)
    |> List.tail
    |> List.sum
    |> should equal 85324
