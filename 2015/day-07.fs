module aoc2015.day_07

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

open FsUnit.Xunit
open Xunit
open System.Text.RegularExpressions
open System

let source = File.load 07


let data =
    [ "123 -> x"
      "456 -> y"
      "x AND y -> d"
      "x OR y -> e"
      "x LSHIFT 2 -> f"
      "y RSHIFT 2 -> g"
      "NOT x -> h"
      "NOT y -> i" ]


let (|Rx|_|) pattern input =
    let m = Regex(pattern).Match(input)

    if m.Success then
        [ for g in m.Groups -> g.Value ]
        |> List.tail
        |> Some
    else
        None

let (|Wire|_|) memory key = Map.tryFind key memory

let parse memory line =
    match line with
    | Rx "^(\d+) -> (\w+)" [ v; t ] -> Map.add t (uint16 v) memory |> Some
    | Rx "^(\w+) -> (\w+)" [ Wire memory a; t ] -> Map.add t a memory |> Some
    | Rx "^(\d+) AND (\w+) -> (\w+)" [ a; Wire memory b; c ] -> Map.add c ((uint16 a) &&& b) memory |> Some
    | Rx "^(\w+) AND (\w+) -> (\w+)" [ Wire memory a; Wire memory b; c ] -> Map.add c (a &&& b) memory |> Some
    | Rx "^(\d+) OR (\w+) -> (\w+)" [ a; Wire memory b; c ] -> Map.add c ((uint16 a) ||| b) memory |> Some
    | Rx "^(\w+) OR (\w+) -> (\w+)" [ Wire memory a; Wire memory b; c ] -> Map.add c (a ||| b) memory |> Some
    | Rx "^(\w+) LSHIFT (\d+) -> (\w+)" [ Wire memory a; b; c ] -> Map.add c (a <<< (int b)) memory |> Some
    | Rx "^(\w+) RSHIFT (\d+) -> (\w+)" [ Wire memory a; b; c ] -> Map.add c (a >>> (int b)) memory |> Some
    | Rx "^NOT (\w+) -> (\w+)" [ Wire memory a; b ] -> Map.add b (~~~a) memory |> Some
    | _ -> None

let rec update memory i (lines : System.Collections.Generic.List<string>) =
    if lines.Count = 0 
    then memory
    else 
        match parse memory lines.[i] with 
        | None -> update memory (i+1) lines
        | Some mem -> 
            lines.RemoveAt i
            update mem 0 lines


[<Fact>]
let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false

[<Fact>]
let ``puzzle 1 is correct`` () =
    let lines = System.Collections.Generic.List<string>(source) 
    update Map.empty 0 lines
    |> Map.find "a"
    |> should equal 3176us

[<Fact>]
let ``puzzle 2 is correct`` () =
    let lines = System.Collections.Generic.List<string>(source) 
    lines.[3] <- "3176 -> b"
    update Map.empty 0 lines
    |> Map.find "a"
    |> should equal 14710us
