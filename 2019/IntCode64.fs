module IntCode64

open System.Collections.Generic

type Parameter =
    | Position of int
    | Immediate of int64
    | Relative of int

type ITerm<'a> =
    abstract Read: unit -> int64
    abstract Write: int64 -> unit
    abstract Output: 'a list

type Program<'a> =
    { Name: string
      Pointer: int
      Register: int
      Term: ITerm<'a>
      Log: (string -> unit) option
      Memory: Dictionary<int, int64> }

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
    type BufferTerm(initial: int64 seq) =
        let mutable buffer = Queue<int64>(initial)

        interface ITerm<int64> with
            member __.Read() = buffer.Dequeue()
            member __.Write i = buffer.Enqueue(i)
            member __.Output = buffer |> Seq.toList

    let Null<'a> =
        { new ITerm<'a> with
            member __.Read() = 0L
            member __.Write i = ()
            member __.Output = []  }

    let createBufferTerm input = BufferTerm(input) :> ITerm<int64>

module Program =
    let Empty<'a> =
        { Pointer = 0
          Register = 0
          Name = ""
          Log = None
          Term = Term.Null<'a>
          Memory = Dictionary<int, int64>() }


    let (~&) p = p.Pointer

    let inline (&.) p i =
        try
            if p.Memory.ContainsKey i then p.Memory.[i]
            else 0L
        with _ -> failwithf "get memory.[%i] != %i" p.Memory.Count i

    let inline (&.+) p i = p &. (i + &p)

    let inline (&.=) p (i, v) =
        try
            if p.Memory.ContainsKey i then p.Memory.[i] <- v
            else p.Memory.Add(i, v)
        with _ -> failwithf "set memory.[%i] != %i" p.Memory.Count i

    let (~&&) p = p.Register
    let inline (&&.+) p i = p &. (&&p + i)

    let param m (i: int64) =
        match m with
        | '2' -> Relative(int i)
        | '1' -> Immediate i 
        | _ -> Position(int i)

    let eval prg =
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
        | s -> failwithf "Uknown op code '%s'" s

    let get p o =
        match o with
        | Immediate v -> (int64) v
        | Position v -> p &. v
        | Relative v -> p &. (&&p + v)

    let set p o x =
        match o with
        | Immediate v -> ()
        | Position v -> p &.= (v, x)
        | Relative v -> p &.= (&&p + v, x)
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
                if (get prg p2) = (get prg p1) then 1L
                else 0L
            set prg p3 v |> inc op
        | JumpIfFalse(p1, p2) ->
            let p =
                if (get prg p1) = 0L then get prg p2 |> int
                else &prg + 3
            { prg with Pointer = p }
        | JumpIfTrue(p1, p2) ->
            let p =
                if (get prg p1) <> 0L then get prg p2 |> int
                else &prg + 3
            { prg with Pointer = p }
        | LessThan(p1, p2, p3) ->
            let v =
                if (get prg p1) < (get prg p2) then 1L
                else 0L
            set prg p3 v |> inc op
        | Offset p ->
            let v = get prg p
            { prg with
                  Pointer = &prg + 2
                  Register = &&prg + (int v) }


    let rec run program =
        let o = eval program
        Option.map (fun t -> sprintf "%A" o |> t) program.Log |> ignore
        match o with
        | Halt -> program
        | _ -> exec program o |> run


    let load (source: string) =
        let d = Dictionary<int, int64>()
        source.Split([| ',' |]) |> Seq.iteri (fun i v -> d.Add(i, (int64 v)))
        d