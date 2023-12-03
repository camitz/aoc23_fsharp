open System
open System.Text.RegularExpressions

type Token = { y: int; x: int; Token: string }

let getTokenInLine (pattern: string) (lineN: int) (line: string) =
    let matches = Regex(pattern).Matches(line)

    [ for m in matches ->
          { y = lineN
            x = m.Index
            Token = m.Value } ]

let isSymbolAdjacentNumber (symbol: Token) (number: Token) =
    if symbol.y < number.y - 1 then false
    elif symbol.y > number.y + 1 then false
    elif symbol.x < number.x - 1 then false
    elif symbol.x > number.x + number.Token.Length then false
    else true

let lines = IO.File.ReadLines(__SOURCE_DIRECTORY__ + @"\input.txt") |> Seq.toList

let engine = array2D (lines |> Seq.map (fun s -> s.ToCharArray()))

let numbers =
    lines
    // |> List.skip 20
    // |> List.take 3
    |> List.mapi (getTokenInLine "[0-9]+")
    |> List.where (fun l -> not l.IsEmpty)
    |> List.collect id

let symbols =
    lines
    // |> List.skip 20
    // |> List.take 3
    |> List.mapi (getTokenInLine "[^.0-9]")
    |> List.where (fun l -> not l.IsEmpty)
    |> List.collect id

let result0 =
    (symbols, numbers)
    ||> List.allPairs
    |> List.where (fun x -> x ||> isSymbolAdjacentNumber)

let result1 =
    result0
    |> List.sort
    |> List.map snd
    |> List.distinct
    |> List.sumBy (fun n -> Int32.Parse n.Token)

printfn $"%A{result1}"

let result2 =
    (result0 |> List.where (fun (s, _) -> s.Token = "*"), numbers)
    ||> List.allPairs
    |> List.where (fun ((_, n1), n2) -> n1 <> n2)
    |> List.where (fun ((s, _), n2) -> isSymbolAdjacentNumber s n2)
    |> List.map (fun ((_, n1), n2) -> (Int32.Parse n1.Token) * (Int32.Parse n2.Token))
    |> List.sum

printfn $"%A{result2/2}"
