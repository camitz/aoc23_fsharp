open System
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections

type Node = Node of string

type Network = Vertice of Network * Network

type Direction =
    | L
    | R

let left = fst
let right = snd

let parseLines (lines: string List) =
    let regex = Regex("([A-Z0-9]{3}) = \(([A-Z0-9]{3}), ([A-Z0-9]{3})\)")

    let rec parseLines' (acc: Map<Node, Node * Node>) (lines: string list) =
        match lines with
        | [] -> acc
        | head :: tail ->
            let m = regex.Match(head)
            let k = Node m.Groups[1].Value
            let v = (Node m.Groups[2].Value, Node m.Groups[3].Value)
            parseLines' (acc |> Map.add k v) tail

    let instruction =
        lines
        |> Seq.head
        |> Seq.map (function
            | 'L' -> L
            | _ -> R)

    let nodes = lines |> List.skip 2 |> (parseLines' Map.empty)

    (instruction, nodes)

let instruction, nodes =
    IO.File.ReadLines(__SOURCE_DIRECTORY__ + @"\input.txt")
    |> Seq.toList
    |> parseLines

let step1 (n: Node) (d: Direction) =
    match d with
    | L -> left nodes[n]
    | _ -> right nodes[n]

let walkInstructionFrom (n: Node) = instruction |> Seq.fold step1 n

let goalNode = (Node "ZZZ")


let rec walkToGoal (goalPred: Node -> bool) (i: int) (n: Node) =
    if goalPred n && i > 0 then
        i
    else
        if i % 1000 = 0 then
            printfn $"{i}"

        walkToGoal goalPred (i + 1) (walkInstructionFrom n)

let isZZZ (n: Node) =
    let (Node s) = n
    s = "ZZZ"

let result = walkToGoal isZZZ 0 (Node "AAA")

printfn $"%A{result * (instruction |> Seq.length)}"

//Part 2

let startNodes =
    nodes |> Map.keys |> Seq.where (fun (Node n) -> n.EndsWith("A")) |> List.ofSeq

let endNodes =
    nodes |> Map.keys |> Seq.where (fun (Node n) -> n.EndsWith("Z")) |> List.ofSeq


let rec walkCycle (n:Node) (i: int) (current: Node) =
    if n = current && i > 1 then
        (i, current)
    else
        walkCycle n (i + 1) (walkInstructionFrom current)

let cycles = [ for n in endNodes -> walkCycle n 0 n ]

printfn $"%A{cycles}"

let rec gcd (a:bigint) (b:bigint) =
    match (a, b) with
    | x, y when y = 0I -> x
    | x, y when x = 0I -> y
    | a, b -> gcd b (a % b)

let lcm a b = a * b / (gcd a b)

let cycleLengths = cycles |> List.map (fst >> bigint)  |> List.fold lcm 1I


let instruction' = instruction |> Seq.toArray
let l = instruction' |> Array.length
let infiniteInstructions =
    Seq.initInfinite (fun i -> instruction'[i%l])

let walkN n (start:Node) =
    infiniteInstructions
    |> Seq.take n
    |> Seq.fold step1 start
    
let walkToZ (start:Node) =
    let mutable c = start

    let l =
        infiniteInstructions
        |> Seq.takeWhile (fun i ->
                c <- step1 c i
                let (Node s) = c
                s.EndsWith("Z") |> not)
        |> Seq.length
    l + 1
        
let distancesToZ =
    startNodes
    |> List.map walkToZ

printfn $"%A{(startNodes, distancesToZ) ||> List.map2 (fun n dist -> walkN dist n)}"
printfn $"%A{(endNodes, cycles |> List.map fst) ||> List.map2 (fun n dist -> walkN (dist*l) n)}"

let distanceToZ =
    distancesToZ
    |> List.map bigint
    |> List.sum

let result1 = ((cycleLengths) * (bigint l)) + distanceToZ
printfn $"%A{cycleLengths}"
printfn $"%A{distanceToZ}"
printfn $"%A{(cycleLengths * (bigint l))}" //<--- this is the right answer for some reason
printfn $"%A{result1}"
