let input =
    System.IO.File.ReadAllLines $"""./inputs/day07.txt""" |> Array.toList

type Hand = { cards: int list; bid: int ; cstring : string }

let f x =
    match x with
    | 'T' -> 10
    | 'J' -> 1
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> int x - 48

let parseHand (string: string) =
    let x = string.Split(" ")

    { cards = x[0] |> List.ofSeq |> List.map f
      bid = int x[1]
      cstring = x[0]}

let score hand =
    let counts = hand.cards |> List.countBy id |> List.map snd |> List.sortDescending

    match counts with
    | [ 5 ] -> 6
    | [ 4; 1 ] -> 5
    | [ 3; 2 ] -> 4
    | [ 3; 1; 1 ] -> 3
    | [ 2; 2; 1 ] -> 2
    | [ 2; 1; 1; 1 ] -> 1
    | [ 1; 1; 1; 1; 1 ] -> 0
    | _ -> -1

let rec tiebreak l1 l2 =
    if List.isEmpty l1 then 0 else
    match List.head l1, List.head l2 with
    | (a, b) when a > b -> 1
    | (a, b) when a < b -> -1
    | (a, b) when a = b -> tiebreak (List.tail l1) (List.tail l2)

let example = [ "32T3K 765"; "T55J5 684"; "KK677 28"; "KTJJT 220"; "QQQJA 483" ]

let score2 hand =
    let baseScore = score hand
    let jokers = hand.cards |> List.filter (fun x -> x = 1) |> List.length
    match baseScore, jokers with
    | _, 0 -> baseScore
    | 6, _ -> 6
    | 5, _ -> 6 // 4ok becomes 5ok (with 1J or 4J)
    | 4, _ -> 6 // FH becomes 5ok (with 2J or 3J)
    | 3, _ -> 5 // 3ok becomes 4ok (with 1J or 3J)
    | 2, _ -> 3 + jokers // 2pair becomes FH (1J) or 4ok (2J)
    | 1, _ -> 3 //2ok becomes 3ok with 1J or 2J
    | 0, _ -> 1 //HC becomes a pair

let rank hand1 hand2 =
    let c = compare (score2 hand1) (score2 hand2)
    if c <> 0 then c else
        printfn "Tiebreak: %s %s" hand1.cstring hand2.cstring 
        tiebreak hand1.cards hand2.cards

input
|> List.map parseHand
|> List.sortWith rank
|> List.mapi (fun i hand -> (i + 1) * hand.bid)
|> List.sum