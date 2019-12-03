open System
open System.IO

let input = 
    Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__.Replace("fsx", "txt"))
    |> File.ReadAllText

let load (s : string) = 
    let m = s.Split([| ','|]) |> Array.map (int)
    0, m

let peek = Array.get 
let poke = Array.set

let op (p, mem) f = 
    let x = peek mem (p+1) |> peek mem
    let y = peek mem (p+2) |> peek mem
    let z = peek mem (p+3) 
    poke mem z (f x y)
    (p+4, mem)

let eval (p, mem)  =
    match peek mem p with
    | 1 -> op (p, mem) (+) |> Some
    | 2 -> op (p, mem) (*) |> Some
    | _ -> None

let rec run program =
    match eval program with
    | None -> peek (snd program) 0
    | Some next -> run next

let mutate x y =
    let p, mem = load input
    poke mem 1 x
    poke mem 2 y
    match run (p, mem) with
    | 19690720 -> Some (x, y)
    | _ -> None

seq { for x in 0..99 do for y in 0..99 do mutate x y}
    |> Seq.choose id
    |> Seq.head
    |> fun (x, y) -> 100 * x + y









