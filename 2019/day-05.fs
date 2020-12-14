module aoc2019.day_05

#if INTERACTIVE
#load @"..\Lib\references.fsx"
#load "IntCode.fs"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

open FsUnit.Xunit
open Xunit
open IntCode

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
    |> Term.output
    |> List.isEmpty
    |> should equal false

    p
    |> Term.output
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
    |> Term.output
    |> List.isEmpty
    |> should equal false

    p
    |> Term.output
    |> List.last
    |> should equal 5893654


[<Theory>]
[<InlineData("3,9,8,9,10,9,4,9,99,-1,8", "8", "1")>]
[<InlineData("3,9,8,9,10,9,4,9,99,-1,8", "7", "0")>]
[<InlineData("3,9,7,9,10,9,4,9,99,-1,8", "7", "1")>]
[<InlineData("3,9,7,9,10,9,4,9,99,-1,8", "8", "0")>]
[<InlineData("3,3,1108,-1,8,3,4,3,99", "8", "1")>]
[<InlineData("3,3,1108,-1,8,3,4,3,99", "7", "0")>]
[<InlineData("3,3,1107,-1,8,3,4,3,99", "7", "1")>]
[<InlineData("3,3,1107,-1,8,3,4,3,99", "8", "0")>]
let comparisonTest (source, input:string, expected:string) =
    Program.load source
    |> Program.attach (Term.single [(int) input])
    |> Program.debug true
    |> Term.output
    |> List.head
    |> should equal (int expected)

"3,9,8,9,10,9,4,9,99,-1,8"
|> Program.load
|> Program.attach (Term.single [7])
|> Program.debug true
