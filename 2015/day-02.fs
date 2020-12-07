module aoc2015.day_02

open Xunit
open Lib

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

open FsUnit.Xunit
let source = File.load 2

let split (exp : string) = exp.Split('x') |> Seq.map (int) 

let calc values = values |> Seq.toList |> function | [l;w;h] -> [ l * w; w * h;  h * l ] |> fun l -> (List.sum l) * 2 + (List.min l)

let ribbon m =  
    let a = m |> Seq.sort |> Seq.take 2 |> Seq.map (fun x -> x + x) |> Seq.sum
    let b = m |> Seq.fold (*) 1
    a + b


[<Fact>]
let ``have source file`` () =  source |> Seq.isEmpty |> should equal false

[<Fact>]
let ``puzzle 1 is correct`` () = 
    source
    |> Seq.map (split >> calc)
    |> Seq.sum
    |> should equal 1586300

[<Fact>]
let ``puzzle 2 is correct`` () = 
    source
    |> Seq.map (split >> ribbon)
    |> Seq.sum
    |> should equal 3737498
