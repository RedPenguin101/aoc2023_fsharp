open System.Text.RegularExpressions
open System.Numerics

let input = System.IO.File.ReadAllLines $"""./inputs/day05.txt"""

let parseIntSeq line =
    let re = Regex(@"\d+")

    re.Matches(line)
    |> Seq.cast<Match>
    |> Array.ofSeq
    |> Array.map (fun m -> BigInteger.Parse m.Value)

let seeds = parseIntSeq input.[0]

let splitArray pred array =
    Array.fold
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

let mapTransform (X: BigInteger array) =
    (X.[1], X.[1] + X.[2] - 1I, X.[0] - X.[1])

let maps =
    splitArray (fun x -> x = "") input.[1..]
    |> List.map (fun l -> l.[.. (l.Length - 3)])
    |> List.rev
    |> List.map (fun l -> List.rev (List.map (parseIntSeq >> mapTransform) l))

let isApplicable x map =
    let f, t, _ = map
    x >= f && x <= t

let applyMap x map =
    let _, _, add = map
    x + add

let chooseAndApply x maps =
    let applicable = List.filter (isApplicable x) maps

    if applicable.Length >= 1 then
        applyMap x applicable[0]
    else
        x

let rec pairup xs =
    match xs with
    | a::b::rest -> (a, b)::pairup rest
    | _ -> []

let seeds2 =
    seeds
    |> Array.toList
    |> pairup
    |> List.map (fun (a,b) -> [a..(a+b)])
    |> List.collect id
    |> List.toArray

// Brute force doesn't work!