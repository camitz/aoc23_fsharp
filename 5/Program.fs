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
                    let [ a; b; c ] = head |> (split " ") |> (Seq.map bigint.Parse) |> List.ofSeq
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

printfn $"%A{mappingSets |> Seq.toList}"

let applyMapping (mappingSet: Mapping seq) (i: bigint) =
    let folder acc (d, s) =
        if fst s <= i && snd s > i then i - fst s + fst d else acc

    (i, mappingSet) ||> Seq.fold folder

let revApplyMapping (mappingSet: Mapping seq) (i: bigint) =
    let folder acc (d, s) =
        if fst d <= i && snd d > i then i - fst d + fst s else acc

    (i, mappingSet) ||> Seq.fold folder

let combinator = (id, mappingSets) ||> Seq.fold (fun f m -> f >> (applyMapping m))

let result = seeds |> Seq.map combinator

printfn $"%A{result |> Seq.min}"

let [ r1; r2; r3; r4; r5; r6; r7 ] =
    mappingSets |> Seq.map revApplyMapping |> Seq.toList

let breaks =
    (([ 0I ] @ (mappingSets |> Seq.item 6 |> Seq.map (fst >> fst) |> Seq.toList))
     |> List.map (r7 >> r6 >> r5 >> r4 >> r3 >> r2 >> r1))
    @ (([ 0I ] @ (mappingSets |> Seq.item 5 |> Seq.map (fst >> fst) |> Seq.toList))
       |> List.map (r6 >> r5 >> r4 >> r3 >> r2 >> r1))
    @ (([ 0I ] @ (mappingSets |> Seq.item 4 |> Seq.map (fst >> fst) |> Seq.toList))
       |> List.map (r5 >> r4 >> r3 >> r2 >> r1))
    @ (([ 0I ] @ (mappingSets |> Seq.item 3 |> Seq.map (fst >> fst) |> Seq.toList))
       |> List.map (r4 >> r3 >> r2 >> r1))
    @ (([ 0I ] @ (mappingSets |> Seq.item 2 |> Seq.map (fst >> fst) |> Seq.toList))
       |> List.map (r3 >> r2 >> r1))
    @ (([ 0I ] @ (mappingSets |> Seq.item 1 |> Seq.map (fst >> fst) |> Seq.toList))
       |> List.map (r2 >> r1))
    @ (([ 0I ] @ (mappingSets |> Seq.item 0 |> Seq.map (fst >> fst) |> Seq.toList))
       |> List.map (r1))

printfn $"f%A{breaks}"
printfn $"r%A{breaks}"

let seedRanges =
    seeds
    |> Seq.toList
    |> List.splitInto ((Seq.length seeds) / 2)
    |> List.map (fun [ l; c ] -> MyRange(l, l + c))

let breaks' =
    breaks
    |> List.where (fun b -> seedRanges |> List.exists (fun (l, u) -> b >= l && b < u))

let result1 = breaks' |> List.map combinator |> List.min

printfn $"%A{result1}"

// let f1 = applyMapping (Seq.head mappingSets)
// let rr1 = revApplyMapping (Seq.head mappingSets)
//
// printfn $"f%A{Seq.map f1 <| seeds}"
// printfn $"f%A{Seq.map (f1 >> r1) <| seeds}"
