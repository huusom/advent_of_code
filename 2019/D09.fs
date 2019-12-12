module Y2019.D09

open IntCode
open Xunit

[<Fact>]
let test() = 
    "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
    |> Program.load
    |> Program.runWithTerm (Term.createBufferTerm Seq.empty)