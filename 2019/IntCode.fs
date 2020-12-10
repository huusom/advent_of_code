module IntCode

type Term =
    | Dummy
    | Single of int list
    | Double of int list * int list
    | Queue of System.Collections.Generic.Queue<int>


type Program =
    { M: int array
      P: int
      T: Term }

module Term =
    let read =
        function
        | Dummy -> 0, Dummy
        | Single (h :: t) -> h, Single t
        | Double ((h :: t), o) -> h, Double(t, o)
        | Queue q -> q.Dequeue(), Queue q

    let write v =
        function
        | Dummy -> Dummy
        | Single l -> Single(l @ [ v ])
        | Double (i, o) -> Double(i, v :: o)
        | Queue q ->
            q.Enqueue v
            Queue q

    let double l = Double(l, [])
    let single l = Single l

    let queue l =
        System.Collections.Generic.Queue<int>(l: int list)
        |> Queue

module Program =
    open Strings

    let load (input: string) =
        { M = input.Split(',') |> Array.map (int)
          P = 0
          T = Dummy }

    let attach t p = { p with T = t }

    type Address =
        | C
        | I of int
        | P of int

    let (>>) prog address =
        match address with
        | C -> prog.P
        | I n -> prog.P + n
        | P n -> prog.M.[prog.P + n]

    let peek prog address =
        let i = prog >> address
        Array.get prog.M (prog >> address)

    let poke prog address value =
        Array.set prog.M (prog >> address) value
        prog

    let read a p =
        let i, t = Term.read p.T
        { poke p a i with T = t }

    let write a p =
        { p with T = Term.write (peek p a) p.T }

    let jump n p = { p with P = peek p n }
    let move n p = { p with P = p.P + n }

    let exec p =
        function
        | "01", (a, b, c) -> (peek p a) + (peek p b) |> poke p c |> move 4
        | "02", (a, b, c) -> (peek p a) * (peek p b) |> poke p c |> move 4
        | "03", (a, _, _) -> p |> read a |> move 2
        | "04", (a, _, _) -> p |> write a |> move 2
        | "05", (a, b, _) -> if (peek p a) <> 0 then jump b p else move 3 p
        | "06", (a, b, _) -> if (peek p a) = 0 then jump b p else move 3 p
        | "07", (a, b, c) ->
            (if (peek p a) < (peek p b) then 1 else 0)
            |> poke p c
            |> move 4
        | "08", (a, b, c) ->
            (if (peek p a) = (peek p b) then 1 else 0)
            |> poke p c
            |> move 4
        | x -> failwithf "unknow operation %A" x

    let eval prog =
        let code = sprintf "%05i" (peek prog C)
        let (par, op) = code / 3

        match par with
        | "000" -> op, (P 1, P 2, P 3)
        | "001" -> op, (I 1, P 2, P 3)
        | "010" -> op, (P 1, I 2, P 3)
        | "011" -> op, (I 1, I 2, P 3)
        | "100" -> op, (P 1, P 2, I 3)
        | "101" -> op, (I 1, P 2, I 3)
        | "110" -> op, (P 1, I 2, I 3)
        | "111" -> op, (I 1, I 2, I 3)
        | _ -> failwithf "unknow code %s at %i" code prog.P

    let rec run prog =
        match eval prog with
        | "99", _ -> prog
        | op -> exec prog op |> run

    let output prog =
        match prog.T with
        | Dummy -> []
        | Single l -> l
        | Double (_, o) -> List.rev o
        | Queue q -> List.ofSeq q