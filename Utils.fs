module Utils

open System.Text.RegularExpressions

let parseIntSeq line =
    let re = Regex(@"\d+")

    re.Matches(line)
    |> Seq.cast<Match>
    |> List.ofSeq
    |> List.map (fun m -> int m.Value)

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

let split pred list =
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
        list
