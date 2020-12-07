module aoc2019.day_07

open FsUnit.Xunit
open Xunit
open Lib
open IntCode

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 07 


[<Fact()>]
let ``have source file`` () =  source |> should not' (be Seq.empty)


let eval program i1 i0 = 
    let p = Program.runWithTerm (Term.createBufferTerm [i0;i1]) program
    p.Term.Output |> List.head

let rec run p =
    let op = Program.eval p
    match op with
    | Output _ -> Program.exec p op
    | Halt -> p
    | _ -> Program.exec p op |> run

let rec start current amps =    
    let p = Array.get amps current |> run
    Array.set amps current p
    let v = p.Term.Read()

    match current with
    | 4 when Program.eval p = Halt -> v
    | 4 -> 
        amps.[0].Term.Write v
        start 0 amps
    | _ -> 
        amps.[current+1].Term.Write v
        start (current+1) amps

let first program phases = 
    Seq.fold (eval program) 0 phases

let second program phases = 
    Term.createBufferTerm [List.head phases; 0 ] :: (phases |> List.tail |> List.map (fun i -> Term.createBufferTerm [i]))
    |> List.mapi (fun i t -> { program with Term = t; Name = sprintf "Amp %i" i })
    |> Array.ofList
    |> start 0


[<Theory>]
[<InlineData("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0", "4,3,2,1,0", 43210)>]
[<InlineData("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0", "0,1,2,3,4", 54321)>]
[<InlineData("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0", "1,0,4,3,2", 65210)>]
let testFirst(source, phases, expected) =
    let prg = Program.load source
    let phs = phases |> Strings.split ',' |> Seq.map (int)
    Assert.Equal(expected, first prg phs)

[<Theory>]
[<InlineData("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5", "9,8,7,6,5", 139629729)>]
[<InlineData("3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10", "9,8,7,6,5", 18216)>]
let testSecond(source, phases, expected) =
    let prg = Program.load source
    let phs = phases |> Strings.split ',' |> Seq.map (int) |> Seq.toList
    Assert.Equal(expected, second prg phs)


[<Fact>]
let evaluate() = 
    let prg = File.text 7 |> Program.load
    let act = Sets.perms [0;1;2;3;4] |> Seq.map (first prg) |> Seq.max
    
    Assert.Equal(844468, act)
   
    
