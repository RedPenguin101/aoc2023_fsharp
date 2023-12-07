open System.Text.RegularExpressions

let input =
    System.IO.File.ReadAllLines $"""./inputs/day01.txt"""
    |> Array.toList

let example2 = """two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen""".Split("\n") |> Array.toList

let reverseString (s:string) = s |> Seq.rev |> System.String.Concat

let numWords = [ "zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
let pattern = List.fold (fun acc s -> acc + "|" + s) "\\d" numWords
let regex = Regex(pattern)

let numWords' = List.map reverseString numWords
let pattern' =  List.fold (fun acc s -> acc + "|" + s) "\\d" numWords'
let regex' = Regex(pattern')

let parse string numList =
    match List.tryFindIndex (fun x -> string = x) numList with
    | Some v -> v
    | None -> int string

let f_l_num s = 
    let fwd_matches = regex.Matches(s)
    let bwd_matches = regex'.Matches(reverseString s)
    10 * parse fwd_matches[0].Value numWords
    +
    parse bwd_matches[0].Value numWords'

let answer = input |> List.map(f_l_num) |> List.sum
printfn "Answer: %A" answer
