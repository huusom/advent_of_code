module IntCode

type Parameter =
    | Position of int
    | Immediate of int

type ITerm =
    abstract Read: unit -> int
    abstract Write: int -> unit
    abstract Output: int list

type Program =
    { Name : string
      Pointer: int
      Term: ITerm
      Memory: int [] }
    with override this.ToString() = sprintf "%s: %i (%A)" this.Name this.Pointer this.Term.Output

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

module Term =
    type BufferTerm(initial : int seq) =
        let mutable buffer = System.Collections.Generic.Queue<int>(initial)

        interface ITerm with

            member __.Read() = buffer.Dequeue()

            member __.Write i = buffer.Enqueue(i)

            member __.Output = buffer |> Seq.cast<int> |> Seq.toList 

    let Null =
        { new ITerm with
            member __.Read() = 0
            member __.Write i = ()
            member __.Output = List.Empty }

    let createBufferTerm input = BufferTerm(input) :> ITerm        

module Program =
    let Empty =
        { Pointer = 0
          Name = ""
          Term = Term.Null
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
            program.Term.Write (read program p)
            { program with Pointer = program.Pointer + 2 }
        | Input p ->
            write program p (program.Term.Read())
            { program with Pointer = program.Pointer + 2 }
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

    let runWithTerm term program = run { program with Term = term }

    let load (source: string) =
        let m = source.Split([| ',' |]) |> Array.map (int)
        { Empty with Memory = m }
