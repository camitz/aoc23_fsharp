module Compact
    open System
    open System.Numerics

    //First some modeling and helpers
    //Defines a lower bound and non-inclusive upper bound
    type Range' = bigint * bigint

    //Tests if i is within range r.
    let isInRange (i:bigint) (r:Range')  =
        fst r <= i && snd r > i

    //Defines a mapping from source to destination for example seed to soil
    type RangeMapping = Range' * Range'

    //Redefine tuple operators and source and dest corresponding to item 2 and 1, as in the input. 
    let dest = fst
    let source = snd

    //Helper to reverse source and destination. Useful in part 2.
    let rev (m:RangeMapping) =
         RangeMapping (source m, dest m)
         
    //A complete mapping set defined on the whole domain for example seed to soil
    type CultivationMapping = RangeMapping list

    let mapIfInRange rm i =
        let mapId i ((dest, source): RangeMapping)  = i - fst source + fst dest
            
        if isInRange i (source rm) then (fun i -> mapId i rm) else id


    let combineFunctions fns = (id, fns) ||> Seq.fold (fun acc fn -> acc >> fn)

    let inline (&|>) fns  i = fns |> Seq.map (fun fn -> fn i)
     
    let parseLines (lines: string seq) =
        let splitBySpace (s: string) =
            s.Split(" ", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

        //The inner line parser threads two accumulators, one for each set of mappings and one for the list of such sets.
        let rec parseMappings (acc: CultivationMapping list) (rangeMapAcc:CultivationMapping) (lines: string list) =
            match lines with
            | [] -> acc @ [rangeMapAcc]
            | head :: tail when head = "" ->
                parseMappings (acc @ [rangeMapAcc]) [] tail
            | head :: tail when head.Contains ":" ->
                parseMappings acc rangeMapAcc tail
            | head :: tail ->
                let [ dest; source; n ] = head |> splitBySpace |> Seq.map bigint.Parse |> List.ofSeq
                parseMappings acc (rangeMapAcc @ [RangeMapping(Range'(dest, dest + n), Range'(source, source + n))]) tail

        Seq.head(lines).Substring("seeds: ".Length) |> splitBySpace |> Seq.map bigint.Parse |> List.ofSeq,
        parseMappings [] [] (lines |> Seq.skip 3 |> List.ofSeq)

    let seeds, cultivationMapping =
        IO.File.ReadLines(__SOURCE_DIRECTORY__ + @"\input.txt") |> parseLines



    //PART 1
    let applyForwardMapping cultivationMapping i =
        let m = cultivationMapping |> Seq.map mapIfInRange &|> i
        m |> combineFunctions <| i
        
    let seedToLocation = cultivationMapping |> Seq.map applyForwardMapping |> combineFunctions

    let resultPart1 = seeds |> Seq.map seedToLocation |> Seq.min

    printfn $"%A{resultPart1}"

    //PART 2
    let applyInverseMapping cultivationMapping i =
        let invMapping = cultivationMapping |> List.map rev
        applyForwardMapping invMapping i

    let inverseCultivationMapping = cultivationMapping |> List.map applyInverseMapping

    let intermediateInverseMappings = ([],inverseCultivationMapping)
                                      ||> List.scan (fun acc f -> acc @ [f])
                                      |> List.skip 1
                                      |> List.map List.rev
                                      |> List.map combineFunctions

    let lowerRangeLimits = cultivationMapping
                          |> List.map (List.map (dest >> fst))
                          |> List.map (fun l -> [0I] @ l)

    let inverseMappedLowerRangeLimits = (lowerRangeLimits, intermediateInverseMappings)
                                        ||> List.map2 (fun low invMapping -> low |> List.map invMapping)
                                        |> List.collect id

    let seedRanges = seeds
                    |> List.splitInto ((Seq.length seeds) / 2)
                    |> List.map (fun [ lower; n ] -> Range'(lower, lower + n))

    let isInSeedDomain seed = seedRanges
                            |> List.exists (isInRange seed)
             
    let resultPart2 = inverseMappedLowerRangeLimits
                      |> List.where isInSeedDomain
                      |> List.map seedToLocation
                      |> List.min

    printfn $"%A{resultPart2}"

