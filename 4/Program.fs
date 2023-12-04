open System
open System.Text.RegularExpressions

let split (sep: string) (s: string) =
    s.Split(sep, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

type Card =
    { ID: int
      WinningNrs: int Set
      YourNrs: int Set }

    static member parse(line: string) =
        let line =
            line.Replace("Card ", "")
            |> split ":"
            |> Array.collect (split "|")
            |> Array.map (split " ")
            |> Array.map (Array.map Int32.Parse)

        { ID = line.[0][0]
          WinningNrs = line.[1] |> Set.ofArray
          YourNrs = line.[2] |> Set.ofArray }

    member this.nWinningNrs =
        (this.WinningNrs, this.YourNrs) ||> Set.intersect
        |> Set.count

    member this.value =
        match this.nWinningNrs with
        | 0 -> 0
        | n -> pown 2 (n-1)

let cards =
    IO.File.ReadLines(__SOURCE_DIRECTORY__ + @"\input.txt")
    |> Seq.toList
    |> List.map Card.parse

let result1 =
    cards
    |> List.map (fun c -> c.value)
    |> List.sum

printfn $"%A{result1}"

let nCardCopies = Array.create cards.Length 1 |> Array.toList

let clone (ns:int list) (at:int) (count:int) (nCopies:int) =
    let add (a:int) (b:int) = a + b

    ns[..at] @
    (ns[at+1..at+count] |> List.map (add nCopies)) @
    ns[at+count+1..]
        
let result2 =
    cards
    |> List.indexed
    |> List.fold (fun acc (i, card) -> clone acc i card.nWinningNrs acc.[i]) nCardCopies
    |> List.sum
    
printfn $"%A{result2}"
