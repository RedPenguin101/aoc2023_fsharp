open System.Text.RegularExpressions

let example = 
  """1abc2
  pqr3stu8vwx
  a1b2c3d4e5f
  treb7uchet""".Split("\n")
  |> Array.toList

let regex = Regex("\\d")

let f_l_num s = 
    let matches = regex.Matches(s)
    int (matches[0].Value + matches[matches.Count - 1].Value)

let input =
    System.IO.File.ReadAllLines $"""./inputs/day01.txt"""
    |> Array.toList

let answer = input |> List.map(f_l_num) |> List.sum
