module IntCode

type Parameter =
    | Position of int
    | Immediate of int

type Program =
    { Pointer: int
      Args: int list
      Memory: int [] }


type Op =
    | Halt
    | Add of Parameter * Parameter * Parameter
    | Mult of Parameter * Parameter * Parameter
    | Input of Parameter
    | Output of Parameter
    | JumpIfTrue of Parameter * Parameter
    | JumpIfFalse of Parameter * Parameter
    | LessThan of Parameter * Parameter * Parameter
    | Equals of Parameter * Parameter * Parameter


let Empty =
    { Pointer = 0
      Args = []
      Memory = [| 0 |] }

let peek index program = program.Memory.[program.Pointer + index]

let parameterOf program index =
    function
    | '1' ->
        program
        |> peek index
        |> Immediate
    | _ ->
        program
        |> peek index
        |> Position

let parameter1 program c = parameterOf program 1 c
let parameter2 program (c1, c2) = parameterOf program 1 c1, parameterOf program 2 c2
let parameter3 program (c1, c2, c3) = parameterOf program 1 c1, parameterOf program 2 c2, parameterOf program 3 c3

let read program =
    function
    | Immediate v -> v
    | Position v -> program.Memory.[v]

let write program parameter v =
    match parameter with
    | Immediate p -> program.Memory.[p] <- v
    | Position p -> program.Memory.[p] <- v

let eval program =
    let instruction =
        program
        |> peek 0
        |> fun i -> i.ToString().PadLeft(5, '0')
        |> Seq.toList
        |> List.rev
    match instruction with
    | '1' :: _ :: m1 :: m2 :: m3 :: _ -> parameter3 program (m1, m2, m3) |> Add
    | '2' :: _ :: m1 :: m2 :: m3 :: _ -> parameter3 program (m1, m2, m3) |> Mult
    | '3' :: _ :: m1 :: _ -> parameter1 program m1 |> Input
    | '4' :: _ :: m1 :: _ -> parameter1 program m1 |> Output
    | '5' :: _ :: m1 :: m2 :: _ -> parameter2 program (m1, m2) |> JumpIfTrue
    | '6' :: _ :: m1 :: m2 :: _ -> parameter2 program (m1, m2) |> JumpIfFalse
    | '7' :: _ :: m1 :: m2 :: m3 :: _ -> parameter3 program (m1, m2, m3) |> LessThan
    | '8' :: _ :: m1 :: m2 :: m3 :: _ -> parameter3 program (m1, m2, m3) |> Equals
    | _ -> Halt

let exec program =
    function
    | Halt -> program
    | Add(p1, p2, p3) ->
        write program p3 ((read program p1) + (read program p2))
        { program with Pointer = program.Pointer + 4 }
    | Mult(p1, p2, p3) ->
        write program p3 ((read program p1) * (read program p2))
        { program with Pointer = program.Pointer + 4 }
    | Output p ->
        printfn "%i" (read program p)
        { program with Pointer = program.Pointer + 2 }
    | Input p ->
        write program p (program.Args.Head)
        { program with
              Pointer = program.Pointer + 2
              Args = program.Args.Tail }
    | Equals(p1, p2, p3) ->
        let v =
            if (read program p2) = (read program p1) then 1
            else 0
        write program p3 v
        { program with Pointer = program.Pointer + 4 }
    | JumpIfFalse(p1, p2) ->
        let p =
            if (read program p1) = 0 then read program p2
            else program.Pointer + 3
        { program with Pointer = p }
    | JumpIfTrue(p1, p2) ->
        let p =
            if (read program p1) <> 0 then read program p2
            else program.Pointer + 3
        { program with Pointer = p }
    | LessThan(p1, p2, p3) ->
        let v =
            if (read program p1) < (read program p2) then 1
            else 0
        write program p3 v
        { program with Pointer = program.Pointer + 4 }


let rec run program =
    let o = eval program
    match o with
    | Halt -> program
    | _ -> exec program o |> run

let runWitArgs program args = run { program with Args = args }

let load (source: string) =
    let m = source.Split([| ',' |]) |> Array.map (int)
    { Empty with Memory = m }

let p =
    load
        "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    
let _ = runWitArgs p [939393]



