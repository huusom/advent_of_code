module IntCode

type Program = {
    Pointer : int
    Memory : int []
}

type Parameter =
    | Position of int
    | Immediate of int

type  Op = 
    | Halt
    | Add of Parameter * Parameter * Parameter
    | Mult of Parameter * Parameter * Parameter
    | Input of Parameter
    | Output of Parameter

module Program = 
    open System
    open AdventOfCode

    let Empty = {
        Pointer = 0
        Memory = [| 0 |]
    }
    
    let peek index program = program.Memory.[program.Pointer + index]

    let toParameter program index = 
        function
        | '1' -> program |> peek index |> Immediate
        | _ -> program |> peek index |> Position

    let read program =
        function 
        | Immediate v -> v
        | Position v -> program.Memory.[v]

    let write program v = 
        function
        | Immediate p -> program.Memory.[p] <- v
        | Position p -> program.Memory.[program.Memory.[p]] <- v


    let eval program = 
        let instruction = 
            program
            |> peek 0
            |> fun i -> i.ToString().PadLeft(5, '0')
            |> Seq.toList
            |> List.rev
        match instruction with 
        | '1'::_::m1::m2::m3::_ -> Add(toParameter program 1 m1, toParameter program 2 m2, toParameter program 3 m3)
        | '2'::_::m1::m2::m3::_ -> Mult(toParameter program 1 m1, toParameter program 2 m2, toParameter program 3 m3)
        | '3'::_::m1::_ -> Input (toParameter program 1 m1)
        | '4'::_::m1::_ -> Output (toParameter program 1 m1)
        | _ -> Halt

    let exec program =
        function
        | Halt -> program
        | Add (p1, p2, p3) -> 
            write program ((read program p1) + (read program p2)) p3
            { program with Pointer = program.Pointer + 4}
        | Mult (p1, p2, p3) -> 
            write program ((read program p1) * (read program p2)) p3
            { program with Pointer = program.Pointer + 4}
        | Output p ->
            printfn "%i" (read program p)
            { program with Pointer = program.Pointer + 2 }
        | Input p ->
            write program p 1
            { program with Pointer = program.Pointer + 2 }

    let load source = Empty