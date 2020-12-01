module Puzzles.day_05

open FsUnit.Xunit
open Xunit
open Lib
open IntCode

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.text 05 


[<Fact()>]
let ``have source file`` () =  source |> should not' (equal System.String.Empty)

[<Fact>]
let testFirst() = 
    let p = 
        source
        |> Program.load
        |> Program.runWithTerm (Term.createBufferTerm [ 1 ])

    Assert.NotEmpty(p.Term.Output)    
    Assert.Equal(9219874, List.last p.Term.Output)

[<Fact>]
let testSecond() =
    let p = 
        source
        |> Program.load
        |> Program.runWithTerm (Term.createBufferTerm [ 5 ])

    Assert.NotEmpty(p.Term.Output)    
    Assert.Equal(5893654, List.head p.Term.Output)

