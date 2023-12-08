﻿open System
open System.Text.RegularExpressions

type Node = string

type Direction =
    | L
    | R
let left = fst
let right = snd

let parseLines (lines: string List) =
    let regex = Regex("([A-Z]{3}) = \(([A-Z]{3}), ([A-Z]{3})\)")

    let rec parseLines' (acc: Map<Node, Node * Node>) (lines: string list) =
        match lines with
        | [] -> acc
        | head :: tail ->
            let m = regex.Match(head)
            let k = Node m.Groups.[1].Value
            let v = (Node m.Groups.[2].Value, Node m.Groups.[3].Value)
            parseLines' (acc |> Map.add k v) tail

    let instruction =
        lines
        |> Seq.head
        |> Seq.map (function
            | 'L' -> L
            | _ -> R)

    let nodes = lines |> List.skip (2) |> (parseLines' Map.empty)

    (instruction, nodes)

let instruction, nodes =
    IO.File.ReadLines(__SOURCE_DIRECTORY__ + @"\testinput.txt")
    |> Seq.toList
    |> parseLines

printfn $"%A{nodes}"

let move (n:Node) (d:Direction) =
    match d with
    | L -> left nodes.[n]
    | _ -> right nodes.[n]
let doInstruction =
    instruction
    |> Seq.fold move (Node "AAA")

printfn $"%A{doInstruction}"