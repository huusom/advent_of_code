module Y2019.D10

let create_map size source =
    source
    |> Seq.filter (function
        | '\r'
        | '\n' -> false
        | _ -> true)
    |> Seq.mapi (fun i c ->
        if c = '.' then None else Some(i % size, i / size))
    |> Seq.choose id
    |> Set.ofSeq

let show_map size map =
    for y in 0 .. (size - 1) do
        for x in 0 .. (size - 1) do
            if Set.contains (x, y) map then printf "#" else printf "."
        printfn ""
    map

let rec gcd =
    function
    | (a, 0) -> abs a
    | (a, b) -> gcd (b, (a % b))

let normalize a b =
    let x = fst b - fst a
    let y = snd a - snd b
    let g = gcd (x, y)
    if g = 0 then (x, y) else (x / g, y / g)

let vectors map a =
    map
    |> Seq.filter ((<>) a)
    |> Seq.map (fun b -> b, (normalize a b))

let visible map a =
    vectors map a
    |> Seq.distinctBy snd
    |> Seq.length

let distances map = map |> Seq.map (fun c -> c, (visible map c))

let asteroids =
    "input\D10.txt"
    |> System.IO.File.ReadAllText
    |> create_map 24

let solve_1 asteroids =
    asteroids
    |> distances
    |> Seq.maxBy snd

let result_1 = solve_1 asteroids

let polar origin a =
    let x = float (fst origin - fst a)
    let y = float (snd origin - snd a)
    match -(atan2 x y) with
    | t when t >= 0.0 -> t
    | t -> 2. * System.Math.PI + t

let length origin a =
    let x = float (fst origin - fst a)
    let y = float (snd origin - snd a)
    sqrt (x * x + y * y)

let sort asteroids station = 
    asteroids
    |> Seq.groupBy (polar station)
    |> Seq.sortBy fst
    |> Seq.map (snd >> Seq.sortBy (length station) )
    |> Seq.cache

let result_2 =  
    let x, y = sort asteroids (fst result_1) |> Seq.item 199 |> Seq.head
    x * 100 + y