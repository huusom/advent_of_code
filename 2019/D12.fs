module Y2019.D12

open System

type Moon =
    { Position: int * int * int
      Velocity: int * int * int }
    static member Empty =
        { Position = 0, 0, 0
          Velocity = 0, 0, 0 }

type Dimension =
    | X
    | Y
    | Z

let parse line =
    match System.Text.RegularExpressions.Regex.Match(line, "<x=([^,]+), y=([^,]+), z=([^,]+)>") with
    | m when not m.Success -> None
    | m ->
        let x = m.Groups.Item(1).Value |> int
        let y = m.Groups.Item(2).Value |> int
        let z = m.Groups.Item(3).Value |> int
        { Moon.Empty with Position = (x, y, z) } |> Some

let add (x1: int, y1: int, z1: int) (x2, y2, z2) = (x1 + x2), (y1 + y2), (z1 + z2)

let inline (!=) (a: int) b =
    if a < b then 1
    elif a = b then 0
    else -1

let delta (x1: int, y1: int, z1: int) (x2, y2, z2) = (x1 != x2), (y1 != y2), (z1 != z2)
let erg (x1: int, y1: int, z1: int) = abs x1 + abs y1 + abs z1

let dim d (x, y, z) =
    match d with
    | X -> x
    | Y -> y
    | Z -> z

let gravity m1 m2 = { m1 with Velocity = add m1.Velocity (delta m1.Position m2.Position) }
let move m1 = { m1 with Position = add m1.Position m1.Velocity }

let show moons =
    for m in moons do
        let x, y, z = m.Position
        let a, b, c = m.Velocity
        printfn "pos=<x=%3i, y=%3i, z=%3i>, vel=<x=%3i, y=%3i, z=%3i>" x y z a b c
    moons

let calculate_next moons = moons |> List.map (fun m -> Seq.fold gravity m moons |> move)

let solution_1 steps moons =
    List.fold (fun m i -> calculate_next m) moons [ 1 .. steps ]
    |> List.sumBy (fun m -> erg m.Position * erg m.Velocity)


let test_1 =
    @"<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>".Split(Environment.NewLine.ToCharArray())
    |> Seq.choose parse
    |> Seq.toList
    |> solution_1 10

let test_2 =
    @"<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>".Split(Environment.NewLine.ToCharArray())
    |> Seq.choose parse
    |> Seq.toList
    |> solution_1 100

let moons =
    System.IO.File.ReadLines "input/D12.txt"
    |> Seq.choose parse
    |> Seq.toList

let answer_1 = solution_1 1000 moons

let rec gcd =
    function
    | (a, 0L) -> abs a
    | (a, b) -> gcd (b, (a % b))

let lcm a b = (a * b) / gcd (a, b)

let find_state moons d =
    let init_p = moons |> List.map (fun m -> dim d m.Position)
    let init_v = moons |> List.map (fun m -> dim d m.Velocity)

    let is_inits moons =
        moons
        |> List.map (fun m -> dim d m.Position)
        |> (=) init_p
        && moons
           |> List.map (fun m -> dim d m.Velocity)
           |> (=) init_v

    let rec look_ahead moons =
        function
        | 0L -> look_ahead (calculate_next moons) 1L
        | n ->
            if is_inits moons
            then n
            else look_ahead (calculate_next moons) (n + 1L)

    look_ahead moons 0L

let answer_2 =
    [ X; Y; Z ]
    |> List.map (find_state moons)
    |> List.reduce lcm
