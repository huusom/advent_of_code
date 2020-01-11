module Y2019.D11

open IntCode64

type State =
    | Paint
    | Move


type Robot(initial) =

    let facing =
        [| (0, -1)
           (1, 0)
           (0, 1)
           (-1, 0) |]

    let mutable state = Paint
    let mutable hull = initial
    let mutable index = 0
    let mutable coordinates = 0,0

    let add a b = 
        (fst a + fst b), (snd a + snd b)

    let paint color =
        hull <- Map.add coordinates color hull
        state <- Move

    let move direction =
        index <- index + if direction = 0 then -1 else 1
        if index < 0 then index <- 3 
        if index > 3 then index <- 0 
        coordinates <- add coordinates facing.[index]
        state <- Paint

    interface ITerm<(int * int) * int64> with

        member __.Read() =
            match Map.tryFind coordinates hull with
            | None -> 0L
            | Some c -> c

        member __.Write i =
            match state with
            | Paint -> paint i
            | Move -> move (int i)

        member __.Output = hull |> Map.toList


let answer_1 = 
    let memory = System.IO.File.ReadAllText "input/D11.txt" |> Program.load

    let prog =
        { Program.Empty with
              Memory = memory
              Term = Robot(Map.empty) }

    let x = Program.run prog
    x.Term.Output |> Seq.length

let answer_2 = 
    let memory = System.IO.File.ReadAllText "input/D11.txt" |> Program.load
    let initial = Map.add (0, 0) 1L Map.empty
    let prog =
        { Program.Empty with
              Memory = memory
              Term = Robot(initial) }

    let x = Program.run prog

    let result = x.Term.Output 

    let min_x = result |> Seq.map (fst >> fst) |> Seq.min
    let max_x = result |> Seq.map (fst >> fst) |> Seq.max
    let min_y = result |> Seq.map (fst >> snd) |> Seq.min
    let max_y = result |> Seq.map (fst >> snd) |> Seq.max

    let map = result |> Map.ofList

    for y in min_y .. max_y do
        for x in min_x .. max_x do
            match Map.tryFind (x, y) map with 
            | Some 0L -> printf "."
            |  _ -> printf "#"
        printfn ""

