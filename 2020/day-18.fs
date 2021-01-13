module aoc2020.day_18

#if INTERACTIVE
#load @"..\Lib\references.fsx"
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#endif

open FsUnit.Xunit
open Xunit
open System.Text.RegularExpressions


let source = File.load 18


let rx = Regex @"\d+|[+*()]"

let tokenize input =
    [ for m in rx.Matches input -> m.Value ]

let prec =
    function
    | "*"
    | "+" -> 1
    | _ -> -1

let prec' =
    function
    | "*" -> 1
    | "+" -> 2
    | _ -> -1

let pop_while p s =
    let rec pop ls =
        function
        | x :: xs when (p x) -> pop (x :: ls) xs
        | xs -> ls, xs

    pop [] s

let shunt_yard_1 (s, q) token =
    // printfn "t:%s s:%10A q:%A" token s q
    match token with
    | "(" -> ("(" :: s), q
    | ")" ->
        match pop_while ((<>) "(") s with
        | a, ("(" :: b)
        | a, b -> b, (a @ q)
    | t when prec t < 0 -> s, (t :: q)
    | op ->
        let splitter op' = (prec op) <= (prec op')
        let a, b = pop_while splitter s
        (op :: b), (a @ q)

let shunt_yard_2 (s, q) token =
    // printfn "t:%s s:%10A q:%A" token s q
    match token with
    | "(" -> ("(" :: s), q
    | ")" ->
        match pop_while ((<>) "(") s with
        | a, ("(" :: b)
        | a, b -> b, (a @ q)
    | t when prec' t < 0 -> s, (t :: q)
    | op ->
        let splitter op' = (prec' op) < (prec' op')
        let a, b = pop_while splitter s
        (op :: b), (a @ q)

let parse shunt tokens =
    tokens
    |> List.fold shunt ([], [])
    |> fun (stack, queue) -> (List.rev queue) @ stack

let reduce op =
    function
    | b :: a :: r -> (op a b) :: r
    | x -> failwithf "wtf: %A" x

let eval s =
    function
    | "+" -> reduce (+) s
    | "*" -> reduce (*) s
    | t -> (int64 t) :: s

let calc shunt =
    tokenize
    >> parse shunt
    >> List.fold eval []
    >> List.head

[<Fact>]
let ``have source file`` () =
    source |> Seq.isEmpty |> should equal false

[<Fact>]
let ``puzzle 1 is correct`` () =
    source
    |> Seq.sumBy (calc shunt_yard_1)
    |> should equal 14006719520523L

[<Fact>]
let ``puzzle 2 is correct`` () =
    source
    |> Seq.sumBy (calc shunt_yard_2)
    |> should equal 545115449981968L


[<Fact>]
let ``basic tests 2`` () =
    let calc_under_test = calc shunt_yard_2

    "1 + 2 * 3 + 4 * 5 + 6"
    |> calc_under_test
    |> should equal 231L

    "1 + (2 * 3) + (4 * (5 + 6))"
    |> calc_under_test
    |> should equal 51L

    "2 * 3 + (4 * 5)"
    |> calc_under_test
    |> should equal 46L

    "5 + (8 * 3 + 9 + 3 * 4 * 3)"
    |> calc_under_test
    |> should equal 1445L

    "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
    |> calc_under_test
    |> should equal 669060L

    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
    |> calc_under_test
    |> should equal 23340L


[<Fact>]
let ``basic tests 1`` () =
    let calc_under_test = calc shunt_yard_1

    "1 + 2 * 3 + 4 * 5 + 6"
    |> calc_under_test
    |> should equal 71L

    "1 + (2 * 3) + (4 * (5 + 6))"
    |> calc_under_test
    |> should equal 51L

    "2 * 3 + (4 * 5)"
    |> calc_under_test
    |> should equal 26L

    "5 + (8 * 3 + 9 + 3 * 4 * 3)"
    |> calc_under_test
    |> should equal 437L

    "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
    |> calc_under_test
    |> should equal 12240L

    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
    |> calc_under_test
    |> should equal 13632L
