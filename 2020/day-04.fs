module aoc2020.day_04

open FsUnit.Xunit
open Xunit

open System.Text.RegularExpressions
open Strings

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 4

let add map (token:string) =
    let kv = token |> split ':'
    Map.add kv.[0] kv.[1] map

let insert map line = line |> split ' ' |> Seq.fold (add) map

let parse passports line =
    match passports with
    | [] -> (insert Map.empty line) :: []
    | _ when line = "" -> Map.empty :: passports
    | m :: rest -> (insert m line) :: rest

let between l u (v: string) = (int v) >= l && (int v) <= u

let rules kv =
    match kv with
    | ("byr", v) -> v |> between 1920 2020
    | ("iyr", v) -> v |> between 2010 2020
    | ("eyr", v) -> v |> between 2020 2030
    | ("hgt", h) ->
        match h /. (h.Length - 2) with
        | (v, "cm") -> v |> between 150 193
        | (v, "in") -> v |> between 59 76
        | _ -> false
    | ("hcl", h) -> h =~ "^#[0-9a-f]{6}$"  //Regex.IsMatch(h, "^#[0-9a-f]{6}$")
    | ("ecl", e) -> e =~ "^(amb|blu|brn|gry|grn|hzl|oth)$"
    | ("pid", p) -> p =~ "^\d{9}$"
    | ("cid", _) -> true
    | _ -> false

let is_valid_1 map =
    match Map.count map with
    | 8 -> true
    | 7 -> map |> Map.containsKey "cid" |> not
    | _ -> false

let is_valid_2 = Map.toSeq >> Seq.forall rules


[<Fact>]
let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false

[<Fact>]
let ``puzzle 1 is correct`` () =
    source
    |> Seq.fold parse []
    |> Seq.filter is_valid_1
    |> Seq.length
    |> should equal 233

[<Fact>]
let ``puzzle 2 is correct`` () =
    source
    |> Seq.fold parse []
    |> Seq.filter is_valid_1
    |> Seq.filter is_valid_2
    |> Seq.length
    |> should equal 111