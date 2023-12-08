open System 

let quadratic a b c =
    let x = sqrt ((b ** 2.0) - (4.0 * a * c))
    (-b + x) / (2.0 * a) , (-b - x) / (2.0 * a)

let solve (d, (t: bigint)) =
    let high, low = quadratic 1.0 (float -d) (float t)
    floor (high - 0.0000001) - ceil (low + 0.0000001) + 1.0

let example  = [(7,9);(15,40);(30,200)]
let input  = [(40,215);(70,1051);(98,2147);(79,1005)]

input
|> List.map (fun (x,y) -> (x, bigint y))
|> List.map solve
|> List.reduce (fun x y -> x * y)
|> printfn "Part 1: %f"

printfn "Part 2: %f" (solve (40709879, 215105121471005I))