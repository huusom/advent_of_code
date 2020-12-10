module Sets

let distrib e L =
    let rec aux pre post =
        seq {
            match post with
            | [] -> yield (L @ [e])
            | h::t -> yield (List.rev pre @ [e] @ post)
                      yield! aux (h::pre) t
        }
    aux [] L

let rec perms = function
| [] -> Seq.singleton []
| h::t -> Seq.collect (distrib h) (perms t)

let rec gcd =
    function
    | (a, 0) -> abs a
    | (a, b) -> gcd (b, (a % b))

let lcm a b = (a * b) / gcd (a, b)
