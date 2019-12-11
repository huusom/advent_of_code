module Y2019.D01

open Xunit

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
        "input/D01.txt"
        |> System.IO.File.ReadLines
        |> Seq.map (int)
        |> Seq.cache
    Assert.Equal (e1, source |> Seq.sumBy first)
    Assert.Equal (e2, source |> Seq.sumBy second)    



 