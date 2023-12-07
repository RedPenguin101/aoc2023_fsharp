open System.Text.RegularExpressions
open System.Numerics

let input =
    System.IO.File.ReadAllLines $"""./inputs/day05example.txt""" |> Array.toList

let parseIntSeq line =
    let re = Regex(@"\d+")

    re.Matches(line)
    |> Seq.cast<Match>
    |> List.ofSeq
    |> List.map (fun m -> BigInteger.Parse m.Value)


let seeds = parseIntSeq input.[0]

let splitArray pred array =
    List.fold
        (fun acc elem ->
            match acc with
            | [] -> [ [ elem ] ]
            | ths :: rest ->
                if pred elem then
                    [ elem ] :: ths :: rest
                else
                    (elem :: ths) :: rest)
        []
        array

let mapTransform (X: BigInteger list) = (X[1], X[1] + X[2] - 1I, X[0] - X[1])

let maps =
    splitArray (fun x -> x = "") input[1..]
    |> List.map (fun l -> l.[.. (l.Length - 3)])
    |> List.rev
    |> List.map (fun l -> List.rev (List.map (parseIntSeq >> mapTransform) l))

let isApplicable x (f, t, _) = x >= f && x <= t
let applyMap x (_, _, add) = x + add

let chooseAndApply x maps =
    let applicable = List.filter (isApplicable x) maps

    if applicable.Length >= 1 then
        applyMap x applicable[0]
    else
        x

List.map (fun x -> List.fold chooseAndApply x maps) seeds |> List.min
