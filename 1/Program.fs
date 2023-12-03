open System
open System.Globalization

let digits = [ 0..9 ]

let parseDigit digitMap s =
    (digitMap
    |> List.findIndex (fun d -> d = s)) % 10 //The position modulo 0 gives the value
    
let findDigits digitMap (s: string) =
    let s = s.ToLower CultureInfo.CurrentCulture
    let indexOf (f:string) = s.IndexOf f
    let lastIndexOf (f:string) = s.LastIndexOf f
    
    let parseDigit = parseDigit digitMap
    
    let getIndex indexFn = 
        digitMap
        |> List.fold (fun acc d -> acc @ [ (indexFn d, d) ]) List.empty        
        |> List.where (fun (i, _) -> i >= 0)
    
    (getIndex indexOf) @ (getIndex lastIndexOf)
    |> List.sort
    |> List.map snd
    |> (fun ds -> (List.head ds, List.last ds))
    |> (fun (d0, d1) -> (parseDigit d0, parseDigit d1))


let digitMap =
        (digits |> List.map string)

let digitMapWithStrings =
    digitMap @
    [ "zero"
      "one"
      "two"
      "three"
      "four"
      "five"
      "six"
      "seven"
      "eight"
      "nine" ]

let findDigitsWithoutStrings = findDigits digitMap
let findDigitsWithStrings = findDigits digitMapWithStrings

let lines = IO.File.ReadLines(__SOURCE_DIRECTORY__ + @"\input.txt") |> Seq.toList
let result =
    lines
    |> List.map findDigitsWithoutStrings
    |> List.map (fun (a,b) -> a * 10 + b)
    |> List.sum

// printfn $"%A{List.zip lines result}"
printfn $"%A{result}"

let result1 =
    lines
    |> List.map findDigitsWithStrings
    |> List.map (fun (a,b) -> a * 10 + b)
    |> List.sum

// printfn $"%A{List.zip lines result}"
printfn $"%A{result1}"
