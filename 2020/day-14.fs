module aoc2020.day_14

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

open FsUnit.Xunit
open Xunit
open Strings

let source = File.load 14

//let int64 (s:string) = System.Convert.ToInt64 s

type Cmd =
    | Mask of string
    | Mem of int64 * int64

let parse line =
    match line with
    | Rx "mask = (\w+)" [ l ] -> Mask l
    | Rx "mem\[(\d+)\] = (\d+)" [ a; i ] -> Mem((int64 a), (int64 i))
    | x -> failwithf "WTF: %A" x


let value_bitmask m i =
    match m with
    | '1' -> '1'
    | '0' -> '0'
    | _ -> i

let address_bitmask m i =
    match m with
    | '0' -> i
    | '1' -> '1'
    | x -> x

let to_string (i: int64) =
    System.Convert.ToString(i, 2).PadLeft(36, '0')

let to_int64 s = System.Convert.ToInt64(s, 2)

let address_mask mask addr =
    addr
    |> to_string
    |> Seq.map2 address_bitmask mask
    |> Seq.toList

let rec unravel chars =
    let add a lst = lst |> List.map (fun lst' -> a :: lst')
    match chars with
    | [] -> [ [ ' ' ] ]
    | 'X' :: rst -> 
        let lst = unravel rst
        List.append (add '0' lst) (add '1' lst)
    | hd :: rst -> add hd (unravel rst)

let add_addresses num mask addr dict = 
    addr 
    |> address_mask mask
    |> unravel
    |> List.map (fun l -> System.String(List.toArray l, 0, 36) |> to_int64)
    |> List.fold (fun d a-> Map.add a num d) dict 
 
let value_mask num mask addr dict =
    let v =
        num
        |> to_string
        |> Seq.map2 value_bitmask mask
        |> Seq.toArray
        |> System.String
        |> to_int64

    Map.add addr v dict


let exec calc (m, dict) =
    function
    | Mask s -> (s, dict)
    | Mem (a, i) -> (m, calc i m a dict)


[<Fact>]
let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false

[<Fact>]
let ``puzzle 1 is correct`` () =
    source
    |> Seq.map parse
    |> Seq.fold (exec value_mask) ("", Map.empty)
    |> snd
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.reduce (+)
    |> should equal 10885823581193L

[<Fact>]
let ``puzzle 2 is correct`` () = 
    source
    |> Seq.map parse
    |> Seq.fold (exec add_addresses) ("", Map.empty)
    |> snd
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.reduce (+)
    |> should equal 3816594901962L
