#load "../Utils.fs"
open System.Numerics

let input =
    System.IO.File.ReadAllLines 
        $"""./inputs/day09.txt""" 
        |> List.ofArray 
        |> List.map Utils.parseBigIntSeq

let x1 = [0; 3; 6; 9; 12; 15] |> List.map BigInteger
let x2 = [1; 3; 6; 10; 15; 21] |> List.map BigInteger
let x3 = [10; 13; 16; 21; 30; 45] |> List.map BigInteger

let rec nextValue sum seq = 
    if List.forall (fun x -> x = 0I) seq then sum
    else
        let newSeq = seq |> List.pairwise |> List.map (fun (x, y) -> y - x)
        nextValue (sum + List.last seq) newSeq

input
|> List.map (nextValue 0I)
|> List.sum
