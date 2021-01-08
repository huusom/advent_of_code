module aoc2020.day_15

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

open FsUnit.Xunit
open Xunit

let source = "12,20,0,6,1,17,7"

type Game =
    { turn: int
      spoken: int
      history: Map<int, int list> }


let parse input =
    let data = input |> Strings.split ','

    { turn = data.Length
      spoken = data |> Array.last |> int
      history =
          data
          |> Seq.mapi (fun i s -> (int s), [ (i + 1) ])
          |> Map.ofSeq }

let speek game =
    match Map.tryFind game.spoken game.history with
    | None -> 0
    | Some l ->
        match l with
        | []
        | [ _ ] -> 0
        | t1 :: t0 :: _ -> t1 - t0

let insert game num =
    let t = game.turn + 1

    match Map.tryFind num game.history with
    | None ->
        { spoken = num
          turn = t
          history = Map.add num [ t ] game.history }
    | Some (t0 :: _) ->
        { spoken = num
          turn = t
          history = Map.add num [ t; t0 ] game.history }

let rec run steps game =
    if steps <= game.turn then
        game.spoken
    else
        game |> speek |> insert game |> run steps


let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false



[<Fact>]
let ``puzzle 1 is correct`` () =
    source |> parse |> run 2020 |> should equal 866


[<Fact(Skip = "Long running")>]
let ``puzzle 2 is correct`` () =
    source
    |> parse
    |> run 30000000
    |> should equal 1437692

[<Fact>]
let ``first set of tests`` () =
    parse "1,3,2" |> run 2020 |> should equal 1
    parse "2,1,3" |> run 2020 |> should equal 10
