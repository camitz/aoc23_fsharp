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

//This is the main domain logic function. It takes a RangeMapping and test whether int i is in the source range.
//If so it will apply the mapping from source to destination.
//If not the int is mapped to itself.
//Partial application defines a function on an int and we will use it in this way together with combineFunctions.
let mapIfInRange (rm:RangeMapping) (i:bigint) =
    let mapId (i:bigint) ((dest, source): RangeMapping)  =
        i - fst source + fst dest
        
    if isInRange i (source rm) then
        (fun i -> mapId i rm)
    else
        id


//Creates a combinator from a list of functions.
//If used together with a CultivationMapping mapped onto mapIfInRange we create something like the following:
//id >> id >> mapping >> id
//This of course equates to just mapping. Points for style here!
let combineFunctions fns =
    (id, fns) ||> Seq.fold (fun acc fn -> acc >> fn)

//Define an apply-operator, in a way the opposite of combineFunctions.
//Takes a list of functions and applies them one by one to a value, producing a list of mapped values.    
let inline (&|>) (fns: ('a -> 'b -> 'c) seq) (i: 'a) =
    fns |> Seq.map (fun fn -> fn i)
 
//The parser
let parseLines (lines: string seq) =
    let split (sep: string) (s: string) =
        s.Split(sep, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
    
    let splitBySpace = split " "

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

    //Return the seeds and all mapping set as a tuple
    Seq.head(lines).Substring("seeds: ".Length) |> splitBySpace |> Seq.map bigint.Parse |> List.ofSeq,
    parseMappings [] [] (lines |> Seq.skip 3 |> List.ofSeq)

//culticationMapping defines the complete domain set of all mappings from seed to location
let seeds, cultivationMapping =
    IO.File.ReadLines(__SOURCE_DIRECTORY__ + @"\input.txt") |> parseLines



//PART 1
//Combining our helpers and model to produce a function taking a mapping set for example seed to soil, and an integer to map.
let applyForwardMapping (cultivationMapping: CultivationMapping) (i: bigint) =
    let m = cultivationMapping |> Seq.map mapIfInRange &|> i
    m |> combineFunctions <| i
    
//Combining all the mappings we get a complete mapping from seed to location.
let seedToLocation = cultivationMapping |> Seq.map applyForwardMapping |> combineFunctions

// All we need to do is apply our mapping to the seeds and get the minimum.
let resultPart1 = seeds |> Seq.map seedToLocation |> Seq.min

printfn $"%A{resultPart1}"



//PART 2

//We start by noting that the complete mapping down to each individual mapping can be (usefully) inverted 
//At least as far as we know prior to having tried it. One precondition is that each RangeMapping is a bijection.
//If they are then the inverse will also be a bijection i.e. useful/trivial.
//And the way to invert the mapping could not be more simple. Just reverse the order of the source/destination.
//This is why we need the the rev function defined in the beginning. 
let applyInverseMapping (cultivationMapping: CultivationMapping) (i: bigint) =
    let invMapping =
        cultivationMapping |> List.map rev
    applyForwardMapping invMapping i

//A list of RangeMappings, defining the complete inverse mapping from location back to seed. 
let inverseCultivationMapping = cultivationMapping |> List.map applyInverseMapping

//Here we need pause for briefing of the general idea.
//
//A brute force attempt will check every possible mapping from seed to location to find the minimum.
//However, it is enough to realize that given a mapping from a seed, adding 1 to that seed map to the same location +1
//i.e. not a minimum UNLESS that mapping encounters a discontinuity in a mapping set (CultivationMapping).
//In other words it is enough to check the lower limits of the ranges, including 0. In other words, the interesting
//things in our mapping happens at, and only at, the discontinuities.
//
//We need to check ALL the lower limits and map them back to seeds to check if they are part of the seed domain.
//Starting from the lower limits of the location destination we map them back through 7 mapping sets to seeds.
//Then the lower limits of humidity and map them back through 6 mapping sets to seeds. And so on.
//
//We can use List.scan for this which is like List.fold but it returns the intermediate results, including the intitial
//case which is the empty list. We must remove that. Then we combine the functions.
//intermediateInverseMappings consists of 7 inverseMappings. The first is the complete mapping. The second is first
//6 combined, all the way to the last which is the inverse of just seed to soil.   
let intermediateInverseMappings =
          ([],inverseCultivationMapping)
          ||> List.scan (fun acc f -> acc @ [f])
          |> List.skip 1
          |> List.map List.rev
          |> List.map combineFunctions

//This is a list of length 7, of the lower range limits. 
let lowerRangeLimits =
                  cultivationMapping
                  |> List.map (List.map (dest >> fst))
                  |> List.map (fun l -> [0I] @ l)

//The above two lists are now to be mapped in conjunction. We can flatten the lists with List.collect.
let inverseMappedLowerRangeLimits =
    (lowerRangeLimits, intermediateInverseMappings)
    ||> List.map2 (fun low invMapping -> low |> List.map invMapping)
    // |> List.skip 2//by increasing this we can check the shortcut conjecture
    // |> List.take 1
    |> List.collect id

//Just a note that the definition of seeds in part 1 as well as part 2 can be viewed as a mapping but different
//than the others. The bijection, if it indeed is one defines contiguous domain and codomain (source and destination),
//from 0 to a large integer. The seed-mapping is not contiguous and will break the bijection. Thus it makes sense to
//treat it separately.
//
//seedRanges redefines seeds into ranges by considering the integers pairwise and constructing ranges. 
let seedRanges =
    seeds
    |> List.splitInto ((Seq.length seeds) / 2)
    |> List.map (fun [ lower; n ] -> Range'(lower, lower + n))

//A helper function using isInRange from before, applying it to a list of Ranges.
let isInSeedDomain (seed:bigint) =
    seedRanges
         |> List.exists (isInRange seed)
         
//With all this in place exclude the inverseMappedLowerLimits not mapped to seed ranges. Then we map them back to
//to the locations with the forward mapping and take the minimum.
let resultPart2 = inverseMappedLowerRangeLimits
                  |> List.where isInSeedDomain
                  |> List.map seedToLocation
                  |> List.min

//This is the result.
printfn $"%A{resultPart2}"

//We our fairly confident about our answer but we can do more to check the bijection assumption
//by checking that we can map the inverseMapped values back and forth from seed to location and
//receive with the same list of values.
assert 
inverseMappedLowerRangeLimits |> List.map seedToLocation |> List.map intermediateInverseMappings[6] =
    inverseMappedLowerRangeLimits

//Note: this is a solution for the general case. Knowing AoC we may suspect the the problem has been constructed in way
//that an obvious shortcut likely solves the it.
//Line 147 marks the spot where we can do some experimenting. We half expect the same minimum to be produces by skipping
//all but the first or all but the last mapping set. This turns out not to be the case. Nonetheless, there is a
//shortcut. If we run the program 7 times, each time skipping one more mapping set, we find that the answer changes
//after 2 and only then. We conclude that the interesting mapping is fertilizer-to-water. It is enough to check only
//the lower limits of these ranges.  