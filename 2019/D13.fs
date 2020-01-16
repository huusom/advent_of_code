module Y2019.D13

open IntCode64

type Game =
    { blocks: int
      ball: int
      paddle: int
      score: int }
    static member Empty =
        { blocks = 0
          ball = 0
          paddle = 0
          score = 0 }

type Arcade() =
    let values = System.Collections.Generic.List<int>()
    let mutable state = Game.Empty

    let rec parse state list =
        match list with
        | [] -> state
        | _ :: _ :: 0 :: tl -> parse state tl // Empty
        | _ :: _ :: 1 :: tl -> parse state tl // Wall
        | _ :: _ :: 2 :: tl -> parse { state with blocks = state.blocks + 1 } tl
        | x :: _ :: 3 :: tl -> parse { state with paddle = x } tl
        | x :: _ :: 4 :: tl -> parse { state with ball = x } tl
        | -1 :: 0 :: v :: tl -> parse { state with score = v } tl
        | _ -> failwithf "Unparseable list %A" list

    let updateGame() =
        state <-
            values
            |> List.ofSeq
            |> parse Game.Empty

        values.Clear()

        if state.ball < state.paddle then -1L
        elif state.ball = state.paddle then 0L
        else 1L

    interface ITerm<Game> with
        member __.Read() = updateGame()
        member __.Write i = values.Add(int i)
        member __.Output =
            values
            |> List.ofSeq
            |> parse state
            |> List.singleton

let answer_1 =
    let memory = System.IO.File.ReadAllText "input/D13.txt" |> Program.load

    let prog =
        { Program.Empty with
              Memory = memory
              Term = Arcade() }
        |> Program.run
    prog.Term.Output.Head


let answer_2 = 
    let memory = System.IO.File.ReadAllText "input/D13.txt" |> Program.load
    memory.[0] <- 2L

    let prog =
        { Program.Empty with
              Memory = memory
              Term = Arcade() }
        |> Program.run

    prog.Term.Output.Head

