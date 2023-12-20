open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines $"""./inputs/day08.txt""" |> Array.toList

let directions = input[0].ToCharArray()

let parseNode string =
    let re = Regex(@"[A-Z]{3}")
    let ms = re.Matches string |> Seq.cast<Match> |> Seq.toList
    (ms[0].Value, (ms[1].Value, ms[2].Value))

let map = input |> List.skip 2 |> List.map parseNode |> Map.ofList

let rec follow idx count position =
    let left, right = map[position]
    let dir = directions[idx]
    let next = if dir = 'L' then left else right

    if next.EndsWith 'Z' then
        (count + 1) |> uint64
    else
        let nextIdx = if idx + 1 = directions.Length then 0 else idx + 1
        follow nextIdx (count + 1) next

let rec gcd a b = if b = 0UL then a else gcd b (a % b)
let lcm a b = (a * b) / (gcd a b)

map
|> Map.keys
|> Seq.filter (fun s -> s.EndsWith 'A')
|> Seq.map (follow 0 0)
|> Seq.reduce lcm
