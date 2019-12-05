module IntCode

type Program = {
    Pointer : int
    Memory : int []
}

type Address =
    | Position of int
    | Immediate of int

type  Op = 
    | Halt
    | Add of Address * Address * Address
    | Mult of Address * Address * Address
    | Input of Address
    | Output of Address

module Program = 
    open System
    open AdventOfCode

    let Empty = {
        Pointer = 0
        Memory = [| 0 |]
    }
    
    let peek pointer program = program.Memory.[pointer]

    let resolve program delta = 
        function
        | '0' -> program |> peek (program)

    let eval program = 
        let instruction = 
            program
            |> peek program.Pointer
            |> fun i -> i.ToString().PadLeft(5, '0')
            |> Seq.toList
            |> List.rev
        match instruction with 
        | '0'::_ -> Halt
        | '1'::_::m1::m2::m3 -> Add(resolve program (1, m1))

    let load source = Empty