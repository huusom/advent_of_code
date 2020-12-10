module aoc2020.day_08

open FsUnit.Xunit
open Xunit

open System
open Strings

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 08

let test =
    [ "nop +0"
      "acc +1"
      "jmp +4"
      "acc +3"
      "jmp -3"
      "acc -99"
      "acc +1"
      "jmp -4"
      "acc +6" ]

type Prog =
    { Memory: string []
      Runs: Set<int>
      Pointer: int
      Acc: int }

module Prog =
    let load input =
        { Memory = Array.ofSeq input
          Runs = Set.empty
          Pointer = 0
          Acc = 0 }


    let jmp n p = { p with Pointer = p.Pointer + n }
    let nop = jmp 1
    let acc n p = { p with Acc = p.Acc + n } |> jmp 1

    let exec p =
        match p.Memory.[p.Pointer] with
        | Rx "nop (.\d+)" [ _ ] -> nop p
        | Rx "acc (.\d+)" [ n ] -> acc (int n) p
        | Rx "jmp (.\d+)" [ n ] -> jmp (int n) p

type Exec =
    | Interminable of Prog
    | Terminated of Prog
    | Valid of Prog

module Exec =
    let eval p =
        if Set.contains p.Pointer p.Runs then
            Interminable p
        elif p.Pointer >= p.Memory.Length then
            Terminated p
        else
            Valid
                { p with
                      Runs = Set.add p.Pointer p.Runs }

    let rec run p =
        match eval p with
        | Valid p -> p |> Prog.exec |> run
        | x -> x

    let valueOf =
        function
        | Interminable p -> p.Acc
        | Terminated p -> p.Acc
        | Valid p -> p.Acc

    let mutate p =
        let insert n s =
            let c = Array.copy p.Memory
            c.[n] <- s
            c

        let change n =
            match p.Memory.[n] with
            | Rx "nop (.\d+)" [ v ] ->
                { p with
                      Memory = insert n (sprintf "jmp %s" v) }
                |> Some
            | Rx "jmp (.\d+)" [ v ] ->
                { p with
                      Memory = insert n (sprintf "nop %s" v) }
                |> Some
            | _ -> None

        seq { 0 .. (p.Memory.Length - 1) }
        |> Seq.choose (change)

    let isTerminated =
        function
        | Terminated _ -> true
        | _ -> false

[<Fact>]
let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false

[<Fact>]
let ``puzzle 1 is correct`` () =
    source
    |> Prog.load
    |> Exec.run
    |> Exec.valueOf
    |> should equal 1766

[<Fact>]
let ``puzzle 2 is correct`` () =
    source
    |> Prog.load
    |> Exec.mutate
    |> Seq.skipWhile (Exec.run >> Exec.isTerminated >> not)
    |> Seq.head
    |> Exec.run
    |> Exec.valueOf
    |> should equal 1639
