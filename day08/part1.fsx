open System.Text.RegularExpressions

let input =
    System.IO.File.ReadAllLines $"""./inputs/day08.txt""" |> Array.toList

let example =
    [ "RL"
      ""
      "AAA = (BBB, CCC)"
      "BBB = (DDD, EEE)"
      "CCC = (ZZZ, GGG)"
      "DDD = (DDD, DDD)"
      "EEE = (EEE, EEE)"
      "GGG = (GGG, GGG)"
      "ZZZ = (ZZZ, ZZZ)" ]

let example2 =
    [ "LLR"; ""; "AAA = (BBB, BBB)"; "BBB = (AAA, ZZZ)"; "ZZZ = (ZZZ, ZZZ)" ]

let directions = input[0].ToCharArray()

let parseNode string = 
    let re = Regex(@"[A-Z]{3}")
    let ms = re.Matches string |> Seq.cast<Match> |> Seq.toList
    (ms[0].Value, (ms[1].Value, ms[2].Value))
    
let map = 
    input 
    |> List.skip 2
    |> List.map parseNode
    |> Map.ofList

let rec follow idx count position = 
    let left, right = map[position]
    let dir = directions[idx]
    let next = if dir = 'L' then left else right

    if next = "ZZZ" then count + 1
    else
    let nextIdx = if idx + 1 = directions.Length then 0 else idx + 1
    follow nextIdx (count + 1) next

follow 0 0 "AAA"
