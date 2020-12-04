module Puzzles.day_04

open FsUnit.Xunit
open Xunit
open Lib
open System.Text.RegularExpressions

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.load 4

let tokenizer =Regex(@"(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):([^ ]+)")

let add map token =
    let m = tokenizer.Match token
    if m.Success
    then Map.add (m.Groups.[1].Value) (m.Groups.[2].Value) map
    else map

let insert map (line: string) = line.Split([| ' ' |]) |> Seq.fold (add) map

let parse passports line =
    match passports with
    | [] -> (insert Map.empty line) :: []
    | _ when line = "" -> Map.empty :: passports
    | m :: rest -> (insert m line) :: rest

let is_valid_1 map =
    match Map.count map with
    | 8 -> true
    | 7 -> map |> Map.containsKey "cid" |> not
    | _ -> false

let between l u (v: string) = (int v) >= l && (int v) <= u

let rules kv =
    match kv with
    | ("byr", v) -> v |> between 1920 2020
    | ("iyr", v) -> v |> between 2010 2020
    | ("eyr", v) -> v |> between 2020 2030
    | ("hgt", h) ->
        let a, b = Strings.substrings (h.Length - 2) h
        match b with
        | "cm" -> a |> between 150 193
        | "in" -> a |> between 59 76
        | _ -> false
    | ("hcl", h) -> Regex.IsMatch(h, "^#[0-9a-f]{6}$")
    | ("ecl", e) -> Regex.IsMatch(e, "^(amb|blu|brn|gry|grn|hzl|oth)$")
    | ("pid", p) -> Regex.IsMatch(p, "^\d{9}$")
    | ("cid", _) -> true
    | _ -> false

let is_valid_2 = Map.toSeq >> Seq.forall rules

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