module Y2019.D14

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open Xunit

let (=!) a b = Assert.Equal(true, (a = b))

let split (c: char) (s: string) =
    s.Split([| c |], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun s -> s.Trim())

let value index (m: Match) = m.Groups.Item(index: int).Value

let parse line =
    let h :: tl =
        Regex.Matches(line, "(\d+) (\w+)")
        |> Seq.cast<Match>
        |> Seq.map (fun m -> (value 2 m), (value 1 m |> int64))
        |> Seq.rev
        |> Seq.toList
    h, (List.rev tl)

let divrem (a: int64) b =
    match Math.DivRem(a, b) with
    | r', 0L -> r', 0L
    | r', s' -> r' + 1L, (b - s')

let find rules name =
    rules
    |> List.find
        (fst
         >> fst
         >> (=) name)

let tryGetValue (inventory: Dictionary<string, int64>) = inventory.TryGetValue

let put inventory name amount =
    match tryGetValue inventory name with
    | (false, _) -> inventory.Add(name, amount)
    | (true, i) -> inventory.[name] <- i + amount

let pull inventory name amount =
    match tryGetValue inventory name with
    | (false, _) -> amount
    | (true, i) ->
        let a = min i amount
        inventory.[name] <- i - a
        amount - a

let rec produce rules inventory =
    function
    | [] -> tryGetValue inventory "ORE" |> snd
    | ("ORE", amount) :: xl ->
        put inventory "ORE" amount
        produce rules inventory xl
    | (name, amount) :: xl ->
        let ((_, batch), bl) = find rules name
        let r, s = divrem (pull inventory name amount) batch
        put inventory name s

        bl
        |> List.map (fun (a, b) -> a, (b * r))
        |> List.append xl
        |> produce rules inventory

let ``solve 1`` rules =

    let inventory = Dictionary<string, int64>()

    produce rules inventory [ ("FUEL", 1L) ]

let ``solve 2`` rules =
    let produce' fuel = produce rules (Dictionary<string, int64>()) [ ("FUEL", fuel) ]
    let target = 1000000000000L

    let rec boundSeek initial result =
        let fuel =
            List.length result + 1
            |> int64
            |> (*) initial

        let ore = produce' (fuel)
        printfn "producing %i fuel uses %i ore" fuel ore
        if ore < target then boundSeek initial (fuel :: result) else (fuel :: result)

    let rec newtonSeek lower upper =
        let next = lower + (upper - lower) / 2L
        let ore = produce' next
        printfn "using %i ore gives %i fuel" ore next
        if ore = target then next
        elif ore > target then newtonSeek lower next
        else newtonSeek next upper

    let u :: l :: _ = boundSeek (target / 1920219L) []
    newtonSeek l u

let input =
    System.IO.File.ReadLines "input/D14.txt"
    |> Seq.map parse
    |> Seq.toList

let ``result 1`` = input |> ``solve 1``

let ``result 2`` = input |> ``solve 2``

module Tests = 
    [<Fact>]
    let ``test 165`` =
        @"9 ORE => 2 A
        8 ORE => 3 B
        7 ORE => 5 C
        3 A, 4 B => 1 AB
        5 B, 7 C => 1 BC
        4 C, 1 A => 1 CA
        2 AB, 3 BC, 4 CA => 1 FUEL"
        |> split '\n'
        |> Seq.map parse
        |> Seq.toList
        |> ``solve 1``
        =! 165L

    [<Fact>]
    let ``test 13312`` =
        @"157 ORE => 5 NZVS
        165 ORE => 6 DCFZ
        44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
        12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
        179 ORE => 7 PSHF
        177 ORE => 5 HKGWZ
        7 DCFZ, 7 PSHF => 2 XJWVT
        165 ORE => 2 GPVTF
        3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
        |> split '\n'
        |> Seq.map parse
        |> Seq.toList
        |> ``solve 1``
        =! 13312L

    [<Fact>]
    let ``test_180697`` =
        @"2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
        17 NVRVD, 3 JNWZP => 8 VPVL
        53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
        22 VJHF, 37 MNCFX => 5 FWMGM
        139 ORE => 4 NVRVD
        144 ORE => 7 JNWZP
        5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
        5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
        145 ORE => 6 MNCFX
        1 NVRVD => 8 CXFTF
        1 VJHF, 6 MNCFX => 4 RFSQX
        176 ORE => 6 VJHF"
        |> split '\n'
        |> Seq.map parse
        |> Seq.toList
        |> ``solve 1``
        =! 180697L

    [<Fact>]
    let ``test 2210736`` =
        @"171 ORE => 8 CNZTR
        7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
        114 ORE => 4 BHXH
        14 VRPVC => 6 BMBT
        6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
        6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
        15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
        13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
        5 BMBT => 4 WPTQ
        189 ORE => 9 KTJDG
        1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
        12 VRPVC, 27 CNZTR => 2 XDBXC
        15 KTJDG, 12 BHXH => 5 XCVML
        3 BHXH, 2 VRPVC => 7 MZWV
        121 ORE => 7 VRPVC
        7 XCVML => 6 RJRHP
        5 BHXH, 4 VRPVC => 5 LTCX"
        |> split '\n'
        |> Seq.map parse
        |> Seq.toList
        |> ``solve 1``
        =! 2210736L

