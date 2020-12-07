module aoc2019.day_01

open Xunit
open FsUnit.Xunit
open Lib

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 1  

let puzzle_1 = 0

let puzzle_2 = 0


let first i = i / 3 - 2

let rec second i =
    let chooser x =
        if x <= 0 then None
        else Some(x, x)
    Seq.unfold (first >> chooser) i |> Seq.sum

[<Theory>]
[<InlineData(1969, 654)>]
[<InlineData(100756, 33583)>]
let testFirst (source, expected) = Assert.Equal(expected, first source)

[<Theory>]
[<InlineData(14, 2)>]
[<InlineData(1969, 966)>]
[<InlineData(100756, 50346)>]
let testSecond (source, expected) = Assert.Equal(expected, second source)

[<Theory>]
[<InlineData(3311492, 4964376)>]
let answers (e1, e2) = 
    let source = 
        File.load 1  
        |> Seq.map (int)
        |> Seq.cache
    Assert.Equal (e1, source |> Seq.sumBy first)
    Assert.Equal (e2, source |> Seq.sumBy second)    


[<Fact>]
let ``have source file`` () =  source |> should not' (be None)

