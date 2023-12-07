open System.Text.RegularExpressions

let input =
    System.IO.File.ReadAllLines $"""./inputs/day02.txt""" |> Array.toList

let example = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".Split("\n") |> Array.toList

let regex = Regex(@"(\d+) (\w)")

let parseInner s =
  regex.Matches(s)
  |> Seq.cast<Match> 
  |> Seq.map (fun (m:Match) -> (m.Groups[2].Value, int m.Groups[1].Value))
  |> Map.ofSeq

let parse (s:string) = 
  s.Split(";")
  |> Array.map(parseInner)
  |> Array.toList

let mergeWith (f : 'B -> 'B -> 'B)  (a : Map<'A, 'B>) (b : Map<'A, 'B>) =
  Map.fold (fun s k v ->
             match Map.tryFind k s with
             | Some v' -> Map.add k (f v v') s
             | None -> Map.add k v s)
    a b

let findOr k m v =
  match Map.tryFind k m with
  | Some v' -> v'
  | None -> v

let isPossible r g b drawn =
  findOr "r" drawn 0 <= r
  && 
  findOr "g" drawn 0 <= g
  &&
  findOr "b" drawn 0 <= b

// 12 r, 13 g, and 14 b. What is the sum of the IDs of those games?

input
|> List.map(parse)
|> List.map(fun xs -> List.reduce (mergeWith max) xs)
|> List.map(isPossible 12 13 14)
|> List.mapi(fun i v -> if v then i+1 else 0)
|> List.sum