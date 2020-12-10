module aoc2015.day_07

open FsUnit.Xunit
open Xunit

open System.Text.RegularExpressions

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

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
    | Rx "^(\d+) -> (\w+)" [ v; t ] -> Map.add t (uint16 v) memory
    | Rx "^(\w+) AND (\w+) -> (\w+)" [ Wire memory a; Wire memory b; c ] -> Map.add c (a &&& b) memory
    | Rx "^(\w+) OR (\w+) -> (\w+)" [ Wire memory a; Wire memory b; c ] -> Map.add c (a ||| b) memory
    | Rx "^(\w+) LSHIFT (\d+) -> (\w+)" [ Wire memory a; b; c ] -> Map.add c (a <<< (int b)) memory
    | Rx "^(\w+) RSHIFT (\d+) -> (\w+)" [ Wire memory a; b; c ] -> Map.add c (a >>> (int b)) memory
    | Rx "^NOT (\w+) -> (\w+)" [ Wire memory a; b ] -> Map.add b (~~~a) memory
    | _ -> memory


let puzzle_1 = 0

let puzzle_2 = 0


let ``have source file`` () = source |> should not' (be None)


let ``puzzle 1 is correct`` () = puzzle_1 |> should equal 0


let ``puzzle 2 is correct`` () = puzzle_2 |> should equal 0
