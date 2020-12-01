module advent_of_code_{year}.day_{day}

open Xunit
open Lib

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = Input.load {day} 

let puzzle_1 = 0

let puzzle_2 = 0


[<Fact>]
let ``have source file`` () = source |> Option.IsSome |> Assert.True

[<Fact>]
let ``puzzle 1 is correct`` () = Assert.NotEqual(puzzle_1, 0)

[<Fact>]
let ``puzzle 2 is correct`` () = Assert.NotEqual(puzzle_2, 0)
