open System
open System.IO

<<<<<<< HEAD
let input = Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__.Replace("fsx", "txt")) |> File.ReadAllText 

let parse (s : string) = s.Split([|','|], StringSplitOptions.RemoveEmptyEntries) |> Array.map (int)

type Op = 
    | Add of int * int * int
    | Mult of int * int * int

type Program (source : string) = 
    let memory = source.Split([|','|], StringSplitOptions.RemoveEmptyEntries) |> Array.map (int)
    let mutable head = 0
    let addresses p = (lookup p+1),()
    let eval f x y z = memory.[z] <- f (memory.[x]) (memory.[y])
    member this.Current with get() = 
        match memory.[head] with
        | 1 -> addresses head |> Option.map Add
        | 2 -> addresses head |> Option.map Mult
        | _ -> None
    member this.Forward() = 
        match this.Current with
        | Some (Add (x, y, z)) -> 
            eval (+) x y z
            head <- head + 4
        | Some (Mult (x, y, z)) ->
            eval (*) x y z
            head <- head + 4
        | None -> ()        


let p = Program "1,1,1,4,99,5,6,0,99"
p.Current
p.Forward ( )
=======
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









>>>>>>> 849b49fd6cb3076d6794fa4f4fd23bc938b89ab2
