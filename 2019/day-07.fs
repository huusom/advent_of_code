module aoc2019.day_07

open FsUnit.Xunit
open Xunit
open IntCode

#if INTERACTIVE
#load @"..\Lib\references.fsx"
#load "IntCode.fs"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.text 07

let run_to_end prg i1 i0 =
    prg
    |> Program.attach (Term.single [ i0; i1 ])
    |> Program.run
    |> Term.output
    |> List.head

let simple_loop prg phases = Seq.fold (run_to_end prg) 0 phases

let feedback_loop prg phases = 0

[<Theory>]
[<InlineData("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0", "4,3,2,1,0", 43210)>]
[<InlineData("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0", "0,1,2,3,4", 54321)>]
[<InlineData("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0",
             "1,0,4,3,2",
             65210)>]
let testFirst (source, phases, expected) =
    phases
    |> Strings.split ','
    |> Seq.map (int)
    |> simple_loop (Program.load source)
    |> should equal expected

[<Theory>]
[<InlineData("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5",
             "9,8,7,6,5",
             139629729)>]
[<InlineData("3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10",
             "9,8,7,6,5",
             18216)>]
let testSecond (source, phases, expected) =
    phases
    |> Strings.split ','
    |> Seq.map (int)
    |> feedback_loop (Program.load source)
    |> should equal expected

[<Fact>]
let have_valid_source_file () =
    source
    |> System.String.IsNullOrWhiteSpace
    |> should equal false

[<Fact>]
let puzzle_1_is_still_valid () =
    let prg = File.text 7 |> Program.load

    [ 0; 1; 2; 3; 4 ]
    |> Sets.perms
    |> Seq.map (simple_loop prg)
    |> Seq.max
    |> should equal 844468
