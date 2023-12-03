open System
open System.Text.RegularExpressions

let split (sep: string) (s: string) =
    s.Split(sep, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

type CubeSet = { Red: int; Green: int; Blue: int }
let defaultCubeSet = { Red = 0; Green = 0; Blue = 0 }
type Game = { ID: int; Reveals: CubeSet list }

let isPossibleReveal (gameSet: CubeSet) (revealSet: CubeSet) =
    gameSet.Red >= revealSet.Red
    && gameSet.Green >= revealSet.Green
    && gameSet.Blue >= revealSet.Blue

let isPossibleGame (gameSet: CubeSet) (game: Game) =
    game.Reveals |> List.forall (isPossibleReveal gameSet)

let minimumGameSet (game: Game) =
    let maxCubeSet (a: CubeSet) (b: CubeSet) =
        { Red = max a.Red b.Red
          Blue = max a.Blue b.Blue
          Green = max a.Green b.Green }

    game.Reveals |> List.reduce maxCubeSet


let parseCubeSet (s: string) =
    let matches = Regex("(\d+) (red|green|blue)").Matches(s)

    matches
    |> Seq.map (fun m -> (Int32.Parse(m.Groups[1].Value), m.Groups[2].Value))
    |> Seq.fold
        (fun acc (n, s) ->
            match s with
            | "red" -> { acc with Red = n }
            | "blue" -> { acc with Blue = n }
            | _ -> { acc with Green = n })
        defaultCubeSet

let parseLine (line: string) =
    line.Replace("Game ", "")
    |> split ":"
    |> Array.collect (split ";")
    |> Array.toList
    |> function
        | g :: b ->
            { ID = Int32.Parse g
              Reveals = b |> List.map parseCubeSet }
        | _ -> failwith ""

let games =
    IO.File.ReadLines(__SOURCE_DIRECTORY__ + @"\input.txt")
    |> Seq.toList
    |> List.map parseLine

let result =
    games
    |> List.where (isPossibleGame { Red = 12; Green = 13; Blue = 14 })
    |> List.sumBy (fun g -> g.ID)

printfn $"%A{result}"

let result2 =
    games |> List.map minimumGameSet |> List.map (fun g -> g.Blue * g.Green * g.Red)
    |> List.sum

printfn $"%A{result2}"
