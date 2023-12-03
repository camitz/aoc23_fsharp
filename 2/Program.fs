open System
open System.Text.RegularExpressions

let split (sep:string) (s:string) = s.Split(sep, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

type blocks = {Red:int; Green:int; Blue:int }
let defaultBlock = {Red=0;Green=0;Blue=0}
type Game =
    { ID:int; Reveals:blocks list }
    
let parseBlocks (s:string) =
   let matches = Regex("(\d+) (red|green|blue)").Matches(s)

   matches
   |> Seq.map (fun m -> (Int32.Parse(m.Groups[1].Value), m.Groups[2].Value))
   |> Seq.fold (fun acc (n, s) ->
                    match s with
                    | "red" -> {acc with Red=n}
                    | "green" -> {acc with Blue=n}
                    | _ -> {acc with Green=n}
                ) defaultBlock 
        
let parseLine (line:string) =
    line.Replace("Game ","")
    |> split ":"
    |> Array.collect (split ";")
    |> Array.toList
    |> function 
        | g::b ->
            { ID = Int32.Parse g
              Reveals =  b
                         |> List.map parseBlocks
            }
        | _ -> failwith ""
let lines = IO.File.ReadLines(__SOURCE_DIRECTORY__ + @"\testinput.txt") |> Seq.toList |> List.map parseLine

printfn $"%A{lines}"
