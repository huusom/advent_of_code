module aoc2019.day_05

open FsUnit.Xunit
open Xunit

open IntCode

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.text 05

[<Fact>]
let ``have source file`` () =
    source
    |> System.String.IsNullOrWhiteSpace
    |> should equal false

[<Fact>]
let testFirst () =
    let p =
        source
        |> Program.load
        |> Program.attach (Term.single [ 1 ])
        |> Program.run

    p
    |> Program.output
    |> List.isEmpty
    |> should equal false

    p
    |> Program.output
    |> List.last
    |> should equal 9219874

[<Fact>]
let testSecond () =
    let p =
        source
        |> Program.load
        |> Program.attach (Term.single [ 5 ])
        |> Program.run

    p
    |> Program.output
    |> List.isEmpty
    |> should equal false

    p
    |> Program.output
    |> List.last
    |> should equal 5893654
