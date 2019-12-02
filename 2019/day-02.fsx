open System
open System.IO

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