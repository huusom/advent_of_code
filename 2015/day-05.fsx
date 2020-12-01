#load "../utils.fsx"

open System
open System.Text.RegularExpressions

let source = utils.load "2015" "05" 

// puzzle 1

let is_nice_1 line = 
    let has_three_vowels = Regex.Matches(line, "[aeiou]").Count >= 3
    let douple_letters = Regex.IsMatch(line, "(\w)\1")
    let not_contains = Regex.IsMatch(line, "(ab|cd|pq|xy)") |> not
    has_three_vowels && douple_letters && not_contains

// test
[ "ugknbfddgicrmopn"
  "aaa"
  "jchzalrnumimnmhp"
  "haegwjzuvuyypxyu"
  "dvszwmarrgswjxmb" ]
  |> List.map is_nice_1

// result
source
    |> Seq.filter is_nice_1
    |> Seq.length

// puzzle 2

let is_nice_2 line = 
    let x = Regex.IsMatch(line, @"(\w\w)\w*\1")
    let y = Regex.IsMatch(line, @"(\w)\w\1") 
    x && y

// test
[ "qjhvhtzxzqqjkmpb"
  "xxyxx"
  "uurcxstgmygtbstg"
  "ieodomkazucvgmuy" ]
  |> List.map is_nice_2

// result
source
    |> Seq.filter is_nice_2
    |> Seq.length
