open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines $"""./inputs/day02.txt"""

let example =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""
        .Split("\n")

let regex = Regex(@"(\d+) (\w)")


let mergeWith (f: 'B -> 'B -> 'B) (a: Map<'A, 'B>) (b: Map<'A, 'B>) =
    Map.fold
        (fun s k v ->
            match Map.tryFind k s with
            | Some v' -> Map.add k (f v v') s
            | None -> Map.add k v s)
        a
        b

let mult = (*)


let parseInner s =
    regex.Matches(s)
    |> Seq.cast<Match>
    |> Seq.map (fun (m: Match) -> (m.Groups[2].Value, int m.Groups[1].Value))
    |> Map.ofSeq

let parse (s: string) = s.Split(";") |> Array.map (parseInner)

let part2 =
    parse >> (Array.reduce (mergeWith max)) >> Map.values >> (Seq.reduce mult)

input |> Array.map (part2) |> Array.sum
