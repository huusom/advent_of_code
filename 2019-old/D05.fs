module Y2019.D05

open IntCode
open System.IO
open Xunit

[<Fact>]
let testFirst() = 
    let p = 
        "input/D05.txt"
        |> File.ReadAllText
        |> Program.load
        |> Program.runWithTerm (Term.createBufferTerm [ 1 ])

    Assert.NotEmpty(p.Term.Output)    
    Assert.Equal(9219874, List.last p.Term.Output)

[<Fact>]
let testSecond() =
    let p = 
        "input/D05.txt"
        |> File.ReadAllText
        |> Program.load
        |> Program.runWithTerm (Term.createBufferTerm [ 5 ])

    Assert.NotEmpty(p.Term.Output)    
    Assert.Equal(5893654, List.head p.Term.Output)
