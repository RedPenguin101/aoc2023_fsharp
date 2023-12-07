#load "../Utils.fs"

type Card = { winning: int Set; your: int Set }

let input = System.IO.File.ReadAllLines $"""./inputs/day04.txt""" |> Array.toList

let example =
    """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""
        .Split("\n")
    |> Array.toList


let scoreCard card =
    let matches = Set.intersect card.winning card.your
    if matches.Count = 0 then 0 else pown 2 (matches.Count - 1)

let cardParse (string: string) =
    let a = string.Split(":").[1].Split("|")

    { winning = Set.ofList (Utils.parseIntSeq a[0])
      your = Set.ofList (Utils.parseIntSeq a[1]) }

input |> List.map (cardParse >> scoreCard) |> List.sum
