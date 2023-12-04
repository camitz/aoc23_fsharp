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

    member this.value =
        let nWinningNrs =
            (this.WinningNrs, this.YourNrs) ||> Set.intersect
            |> Set.count
            
        match nWinningNrs with
        | 0 -> 0
        | n -> pown 2 (n-1)

let parseCubeSet (s: string) =
    let matches = Regex("\d+").Matches(s)

    matches |> Seq.map (fun m -> Int32.Parse(m.Value))

let cards =
    IO.File.ReadLines(__SOURCE_DIRECTORY__ + @"\input.txt")
    |> Seq.toList
    |> List.map Card.parse
    |> List.map (fun c -> c.value)
    |> List.sum

printfn $"%A{cards}"
