module aoc2015.day_04

open FsUnit.Xunit
open Xunit
open Lib
open System
open System.Text
open System.Security.Cryptography

#if INTERACTIVE
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

let source = File.text 04

let md5 = MD5.Create()

let hash (s: string) =
    Encoding.UTF8.GetBytes s
    |> md5.ComputeHash
    |> BitConverter.ToString

let is_match secret prefix i =
    (sprintf "%s%i" secret i |> hash)
        .StartsWith(prefix: string)


[<Fact>]
let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false


[<Fact>]
let ``puzzle 1 is correct`` () =
    seq { 0 .. 10000000 }
    |> Seq.find (is_match source "00-00-0")
    |> should equal 282749


[<Fact>]
let ``puzzle 2 is correct`` () =
    seq { 0 .. 10000000 }
    |> Seq.tryFind (is_match source "00-00-00")
    |> should equal (Some 9962624)
