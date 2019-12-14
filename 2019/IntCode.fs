module IntCode

open System

type Parameter =
    | Position of int
    | Immediate of int
    | Relative of int

type ITerm =
    abstract Read: unit -> int
    abstract Write: int -> unit
    abstract Output: int list

type Program =
    { Name: string
      Pointer: int
      Register: int
      Term: ITerm
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
    | Offset of Parameter

module Term =
    type BufferTerm(initial: int seq) =
        let mutable buffer = System.Collections.Generic.Queue<int>(initial)

        interface ITerm with
            member __.Read() = buffer.Dequeue()
            member __.Write i = buffer.Enqueue(i)
            member __.Output =
                buffer
                |> Seq.cast<int>
                |> Seq.toList

    let Null =
        { new ITerm with
            member __.Read() = 0
            member __.Write i = ()
            member __.Output = List.Empty }

    let createBufferTerm input = BufferTerm(input) :> ITerm

module Program =
    let Empty =
        { Pointer = 0
          Register = 0
          Name = ""
          Term = Term.Null
          Memory = [| 0 |] }


    let (~&) p = p.Pointer
    let inline (&.) p i = try   p.Memory.[i] with _ -> failwithf "get memory.[%i] != %i" p.Memory.Length i 
    let inline (&.+) p i = p &. (i + &p)
    let inline (&.=) p (i, v) = try  p.Memory.[i] <- v with _ -> failwithf "set memory.[%i] != %i" p.Memory.Length i 

    let (~&&) p = p.Register
    let inline (&&.+) p i = p &. (&&p + i)

    let param m i =
        match m with
        | '2' -> Relative i
        | '1' -> Immediate i
        | _ -> Position i

    let eval prg =
        let p = prg &. &prg
        let cmd = (prg &. &prg).ToString().PadLeft(5, '0')

        match cmd.Substring(3) with
        | "99" -> Halt
        | "01" -> Add(param cmd.[2] (prg &.+ 1), param cmd.[1] (prg &.+ 2), param cmd.[0] (prg &.+ 3))
        | "02" -> Mult(param cmd.[2] (prg &.+ 1), param cmd.[1] (prg &.+ 2), param cmd.[0] (prg &.+ 3))
        | "03" -> Input(param cmd.[2] (prg &.+ 1))
        | "04" -> Output(param cmd.[2] (prg &.+ 1))
        | "05" -> JumpIfTrue(param cmd.[2] (prg &.+ 1), param cmd.[1] (prg &.+ 2))
        | "06" -> JumpIfFalse(param cmd.[2] (prg &.+ 1), param cmd.[1] (prg &.+ 2))
        | "07" -> LessThan(param cmd.[2] (prg &.+ 1), param cmd.[1] (prg &.+ 2), param cmd.[0] (prg &.+ 3))
        | "08" -> Equals(param cmd.[2] (prg &.+ 1), param cmd.[1] (prg &.+ 2), param cmd.[0] (prg &.+ 3))
        | "09" -> Offset(param cmd.[2] (prg &.+ 1))

    let get p o =
        match o with
        | Immediate v -> v
        | Position v -> p &. v
        | Relative v -> p &. (&&p + v)

    let set p o x =
        match o with
        | Immediate v -> ()
        | Position v -> p &.= (v, x)
        | Relative v -> p &.= (&&p+v, x)
        p

    let inc o p =
        match o with
        | Offset _
        | Halt -> p
        | Add _
        | Equals _
        | LessThan _
        | Mult _ -> { p with Pointer = &p + 4 }
        | Output _
        | Input _ -> { p with Pointer = &p + 2 }
        | JumpIfFalse _
        | JumpIfTrue _ -> { p with Pointer = &p + 3 }


    let exec prg op =
        match op with
        | Halt -> prg
        | Add(p1, p2, p3) ->
            (get prg p1) + (get prg p2)
            |> set prg p3
            |> inc op
        | Mult(p1, p2, p3) ->
            (get prg p1) * (get prg p2)
            |> set prg p3
            |> inc op
        | Output p ->
            prg.Term.Write(get prg p)
            inc op prg
        | Input p -> set prg p (prg.Term.Read()) |> inc op
        | Equals(p1, p2, p3) ->
            let v =
                if (get prg p2) = (get prg p1) then 1
                else 0
            set prg p3 v |> inc op
        | JumpIfFalse(p1, p2) ->
            let p =
                if (get prg p1) = 0 then get prg p2
                else &prg + 3
            { prg with Pointer = p }
        | JumpIfTrue(p1, p2) ->
            let p =
                if (get prg p1) <> 0 then get prg p2
                else &prg + 3
            { prg with Pointer = p }
        | LessThan(p1, p2, p3) ->
            let v =
                if (get prg p1) < (get prg p2) then 1
                else 0
            set prg p3 v |> inc op
        | Offset p ->
            let v = get prg p
            { prg with
                  Pointer = &prg + 2
                  Register = &&prg + v }


    let rec run program =
        let o = eval program
        match o with
        | Halt -> program
        | _ -> exec program o |> run

    let runWithTerm term program = run { program with Term = term }

    let load (source: string) =
        let m = source.Split([| ',' |]) |> Array.map (int)
        { Empty with Memory = m }
