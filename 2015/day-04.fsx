#load "../utils.fsx"

open System
open System.Text
open System.Security.Cryptography

let md5 = MD5.Create()

let hash (s : string) =
    Encoding.UTF8.GetBytes s
    |> md5.ComputeHash 
    |> BitConverter.ToString

let is_match secret prefix i = sprintf "%s%i" secret i |> hash |> utils.startsWith prefix

let source = utils.load "2015" "04" |> Seq.head

// puzzle 1
seq { 0 .. 10000000} |> Seq.find (is_match source "00-00-0")

// puzzle 2
seq { 0 .. 10000000} |> Seq.tryFind (is_match source "00-00-00")



