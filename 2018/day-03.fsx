#load @"..\utils.fsx"

let createClaim [| i; x; y; w; h |]  =  i, seq { for x' in x..(x+w-1) do for y' in y..(y+h-1) do x', y'}
    
let add canvas (_, coords) = 
    for (x, y) in coords do
        let v = Array2D.get canvas x y
        Array2D.set canvas x y (v+1)
    canvas

let toSeq (canvas : int[,]) =
    let w = Array2D.length1 canvas - 1
    let h = Array2D.length2 canvas - 1
    seq {
        for x in 0..w do
        for y in 0..h do
        Array2D.get canvas x y
    }

let exists canvas (_, coords) = 
    coords
    |> Seq.exists (fun (x, y) -> (Array2D.get canvas x y ) > 1 )
    |> not

let claims = 
    Utils.combine __SOURCE_DIRECTORY__ __SOURCE_FILE__ 
    |> Utils.replace ".fsx" ".txt"
    |> Utils.parse @"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"
    |> Option.get
    |> Seq.map ((int) |> Array.map >> createClaim)
    |> Seq.cache

let canvas = 
    claims
    |> Seq.fold add (Array2D.create 1000 1000 0)

let count = 
    canvas
    |> toSeq
    |> Seq.filter (fun i -> i > 1)
    |> Seq.length

let single =
    claims
    |> Seq.filter (exists canvas)
    |> Seq.head
    |> fst