let input =
    System.IO.File.ReadAllLines $"""./inputs/day07.txt""" |> Array.toList

type Hand = { cards: int list; bid: int }

let f x =
    match x with
    | 'T' -> 10
    | 'J' -> 11
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> int x - 48

let parseHand (string: string) =
    let x = string.Split(" ")

    { cards = x[0] |> List.ofSeq |> List.map f
      bid = int x[1] }

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
    match List.head l1, List.head l2 with
    | (a, b) when a > b -> 1
    | (a, b) when a < b -> -1
    | (a, b) when a = b -> tiebreak (List.tail l1) (List.tail l2)

let example = [ "32T3K 765"; "T55J5 684"; "KK677 28"; "KTJJT 220"; "QQQJA 483" ]

let rank hand1 hand2 =
    if (score hand1) > (score hand2) then 1
    elif (score hand1) < (score hand2) then -1
    else tiebreak hand1.cards hand2.cards

input
|> List.map (parseHand)
|> List.sortWith rank
|> List.mapi (fun i hand -> (i + 1) * hand.bid)
|> List.sum
