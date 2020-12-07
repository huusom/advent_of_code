module aoc2019.day_09

open FsUnit.Xunit
open Xunit
open Lib
open Xunit.Abstractions
open IntCode64

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

type Tests(helper: ITestOutputHelper) =
    [<Theory>]
    [<InlineData("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99", "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")>]
    [<InlineData("1102,34915192,34915192,7,4,7,99,0", "1219070632396864")>]
    [<InlineData("104,1125899906842624,99", "1125899906842624")>]
    let examples(source, expected) =

        let program =
            Program.run
                { Program.Empty<int64> with
                      Memory = Program.load source
                      Term = Term.createBufferTerm Seq.empty
                      Log = Some helper.WriteLine }

        let actual =
            program.Term.Output
            |> Seq.map (sprintf "%i")
            |> String.concat ","


        Assert.Equal(expected, actual)


    [<Fact>] 
    let firstPuzzle () = 
        let prg = 
            Program.run
                { Program.Empty with
                    Memory = File.text 9 |> Program.load
                    Term = Term.createBufferTerm (Seq.singleton 1L) }

        for x in prg.Term.Output do
            helper.WriteLine(x.ToString())
        
        Assert.Equal(3063082071L, prg.Term.Output.Head)

    [<Fact>] 
    let secondPuzzle () = 
        let prg = 
            Program.run
                { Program.Empty with
                    Memory = File.text 9 |> Program.load
                    Term = Term.createBufferTerm (Seq.singleton 2L) }

        for x in prg.Term.Output do
            helper.WriteLine(x.ToString())
        
        Assert.Equal(81348L, prg.Term.Output.Head)
