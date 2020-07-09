module Urification

open System.Text

(*
3- encode empty spaces in given string with %20. You may assume that string has sufficient space for this.
You are given a 'true' length of string (the length of actual text with spaces).

input1 =  "i am going for a walk          "
result1 = "i%20am%20going%20for%20a%20walk"

Assumption: string length has exact amount of space needed for encoding
1- create string builder ( O(1) )
2- iterate s chars and push %20 or chars to sb ( O(s.length + (s.length * 1) ) )
time: O(1 + s.length + s.length) -> O(1 + 2n) -> O(n)
space: O(2n) -> O(n) 

Assumption: string length has exact amount of space needed for encoding    
1- allocate new array of s.length ( O(s.length) )
2- iterate s chars and replace spaces with %20 in new array ( O(s.length) )
time: O(s.length + s.length) -> O(2n) -> O(n)
space: O(2n) -> O(n)

No assumptions...
1- get number of spaces in string
2- using it, determine unneeded spaces to fill with nulls
3- iterate backwards
time: O(trueLength + s.length) -> O(n)
space: O(n)

*)

let encodeSb (str : string) =
    let rec loop chars index (sb : StringBuilder) =
        match chars with
        | [] -> sb |> string
        | _ when str.Length = index -> sb |> string
        | head::tail ->
            if head = ' ' then
                sb.Append '%' |> ignore
                sb.Append '2' |> ignore
                sb.Append '0' |> ignore
                loop tail (index + 3) sb
            else
                sb.Append head |> ignore
                loop tail (index + 1) sb
    
    loop (str |> Seq.toList) 0 (StringBuilder())
   
let encodeArray (str : string) =       
    let rec loop chars index (arr : char []) =
        match chars with
        | [] -> System.String(arr)
        | _ when str.Length = index -> System.String(arr)
        | head::tail ->
            if head = ' ' then
                arr.[index] <- '%'
                arr.[index + 1] <- '2'
                arr.[index + 2] <- '0'
                loop tail (index + 3) arr
            else
                arr.[index] <- head
                loop tail (index + 1) arr
    loop (str |> Seq.toList) 0 (Array.zeroCreate str.Length)
    
let encodeArrayCorrect (str : string) (trueLength : int) =
    let arr = str.ToCharArray()
    let countSpaces (arr : char []) (take : int) = arr |> Array.take take |> Array.filter (fun c -> c = ' ') |> Array.length
    let mutable newIndex = trueLength - 1 + ((countSpaces arr trueLength)  * 2)
    
    // add nulls for unused spaces
    
    for oldIndex in trueLength-1..-1..0 do
        if arr.[oldIndex] = ' ' then
            arr.[newIndex] <- '0'
            arr.[newIndex-1] <- '2'
            arr.[newIndex-2] <- '%'
            newIndex <- newIndex - 3
        else
            arr.[newIndex] <- arr.[oldIndex]
            newIndex <- newIndex - 1
            
    System.String(arr)

