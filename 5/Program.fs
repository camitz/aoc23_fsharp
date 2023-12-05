open System
open System.Numerics

let split (sep: string) (s: string) =
    s.Split(sep, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

type MyRange = bigint * bigint
type Mapping = MyRange * MyRange
type Seeds = bigint seq

let parseLines (lines: string seq) =
    let seeds = Seq.head(lines).Substring(7) |> split (" ") |> Seq.map bigint.Parse

    let rec parseMappings (lines: string list) =
        let rec parseMapping (lines: string list) =
            seq {
                match lines with
                | head :: _ when head = "" -> ()
                | head :: tail ->
                    let  [a; b; c ] = head |> (split " ") |> (Seq.map bigint.Parse) |> List.ofSeq
                    yield Mapping(MyRange(a, a + c), MyRange(b, b + c))
                    yield! (parseMapping tail)
                | _ -> ()
            }

        seq {
            let mutable lines' = lines
            while lines'.Length > 1 do 
                let t = parseMapping (lines' |> List.skip 2)
                lines' <- lines' |> List.skip (2 + Seq.length t)
                yield t
        }

    (seeds, parseMappings (lines |> Seq.skip 1 |> List.ofSeq))

let seeds, mappingSets =
    IO.File.ReadLines(__SOURCE_DIRECTORY__ + @"\input.txt") |> parseLines

// printfn $"%A{mappingSets |> Seq.toList}"

let applyMapping (mappingSet: Mapping seq) (i: bigint) =
    let folder acc (d, s) =
        // printfn $"%A{acc}"
        if fst s <= i && snd s > i then i - fst s + fst d else acc
        
    (i, mappingSet)
    ||> Seq.fold folder

let combinator =
    (id, mappingSets)
    ||> Seq.fold (fun f m -> f >> (applyMapping m) )

let result =
    seeds
    |> Seq.map combinator
    
printfn $"%A{result |> Seq.min}"

