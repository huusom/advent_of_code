module IntCode

type sz = int

type Term =
    | Dummy
    | Single of sz list
    | Double of sz list * sz list
    | Queue of System.Collections.Generic.Queue<sz>

type Op =
    | STOP
    | ADD of char list
    | MULT of char list
    | IN of char list
    | OUT of char list
    | JZ of char list
    | JNZ of char list
    | LT of char list
    | EQ of char list
    | UNK


type Program = { M: sz array; P: sz; T: Term }

module Term =
    let read =
        function
        | Dummy -> 0, Dummy
        | Single (h :: t) -> h, Single t
        | Double ((h :: t), o) -> h, Double(t, o)
        | Queue q -> q.Dequeue(), Queue q
        | x -> failwithf "WTF: %A" x

    let write v =
        function
        | Dummy -> Dummy
        | Single l -> Single(l @ [ v ])
        | Double (i, o) -> Double(i, v :: o)
        | Queue q ->
            q.Enqueue v
            Queue q

    let output prg =
        match prg.T with
        | Dummy -> []
        | Single l -> l
        | Double (_, o) -> List.rev o
        | Queue q -> List.ofSeq q

    let double l = Double(l, [])
    let single l = Single l

    let queue l =
        System.Collections.Generic.Queue<sz>(l: sz list)
        |> Queue

module Op =
    open Strings

    let map str = str |> Seq.rev |> Seq.toList

    let eval code =
        match code /. 3 with
        | _, "99" -> STOP
        | par, "01" -> map par |> ADD
        | par, "02" -> map par |> MULT
        | par, "03" -> map par |> IN
        | par, "04" -> map par |> OUT
        | par, "05" -> map par |> JNZ
        | par, "06" -> map par |> JZ
        | par, "07" -> map par |> LT
        | par, "08" -> map par |> EQ
        | _ -> UNK

module Program =
    open Strings

    let load (input: string) =
        { M = input.Split(',') |> Array.map (int)
          P = 0
          T = Dummy }

    let attach t p = { p with T = t }

    let (~&) prg = prg.P
    let (>>) prg n = Array.get prg.M (&prg + n)
    let (>>>) prg n = (prg >> n) |> Array.get prg.M

    let peek prg n =
        function
        | '0' -> (prg >>> n)
        | _ -> (prg >> n)

    let poke prg n v = Array.set prg.M (prg >> n) v

    let addition p0 p1 prg =
        let a = peek prg 1 p0
        let b = peek prg 2 p1
        poke prg 3 (a + b)
        { prg with P = &prg + 4 }

    let multiply p0 p1 prg =
        let a = peek prg 1 p0
        let b = peek prg 2 p1
        poke prg 3 (a * b)
        { prg with P = &prg + 4 }

    let input prg =
        let v, t = Term.read prg.T
        poke prg 1 v
        { prg with P = &prg + 2; T = t }

    let output p0 prg =
        let a = peek prg 1 p0
        let t = Term.write a prg.T
        { prg with P = &prg + 2; T = t }

    let jump_if_true p0 p1 prg =
        let a = peek prg 1 p0
        let b = peek prg 2 p1
        if a <> 0 then { prg with P = b } else { prg with P = &prg + 3 }

    let jump_if_not_true p0 p1 prg =
        let a = peek prg 1 p0
        let b = peek prg 2 p1
        if a = 0 then { prg with P = b } else { prg with P = &prg + 3 }

    let less_than p0 p1 p2 prg =
        let a = peek prg 1 p0
        let b = peek prg 2 p1
        let c = peek prg 3 '1'
        if a < b then prg.M.[c] <- 1 else prg.M.[c] <- 0
        { prg with P = &prg + 4 }

    let equal p0 p1 p2 prg =
        let a = peek prg 1 p0
        let b = peek prg 2 p1
        let c = peek prg 3 '1'
        if a = b then prg.M.[c] <- 1 else prg.M.[c] <- 0
        { prg with P = &prg + 4 }


    let exec prg =
        function
        | ADD [ a; b; _ ] -> prg |> addition a b
        | MULT [ a; b; _ ] -> prg |> multiply a b
        | IN [ _; _; _ ] -> prg |> input
        | OUT [ a; _; _ ] -> prg |> output a
        | JZ [ a; b; _ ] -> prg |> jump_if_true a b
        | JNZ [ a; b; _ ] -> prg |> jump_if_not_true a b
        | LT [ a; b; c ] -> prg |> less_than a b c
        | EQ [ a; b; c ] -> prg |> equal a b c
        | x -> failwithf "WTF: %A" x


    let rec debug enabled prg =
        let code = sprintf "%05i" (prg >> 0)
        let op = Op.eval code

        if (enabled) then printfn "[%05i] %A" &prg op

        match op with
        | STOP -> prg
        | UNK -> failwithf "Uknown code '%s' at %i" code prg.P
        | _ -> exec prg op |> debug enabled

    let run = debug false
