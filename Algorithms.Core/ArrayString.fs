module ArrayString

open System
open System.Text

module Uniqueness =
    // 1- Write an algorithm to determine if string has all unique characters. Try not to use any data structures.

    // assuming string is ASCII chars, hence 128 size array
    let hasUniqueCharsArray (str : string) =
        let rec loop (chars : char list) (arr : bool[]) =
            match chars with
            | [] -> true
            | head::tail ->
                let num = head |> int
                if arr.[num] = true then false
                else
                    arr.[num] <- true
                    loop tail arr
        loop (str |> Seq.toList) (Array.init 128 (fun _ -> false))
        
    // using hashset
    let hasUniqueCharsHash (str : string) =
        let rec loop (chars : char list) (set : Set<char>) =
            match chars with
            | [] -> true
            | head::tail ->
                if set.Contains head then false
                else loop tail (set |> Set.add head)
        
        loop (str |> Seq.toList) Set.empty
        
module Permutation =    
    // 2- Given two strings, write a method to decide if one is a permutation of the other.
    (*
        case1: true
        str1 = jonas
        str2 = sanoj
        
        case2: false
        str1 = jonas
        str2 = jono
        
        solution0: create array of char counts, then iterate second str and take out char counts
        assuming: ASCII char set
        time: O(s1.length + s2.length) - best
        
        solution1: create hashmap of chars + counts of both strings, compare counts
        time:
        - create map of s1 and s2 = O(s1.length + s2.length)
        - compare = O(max(m1.length, m2.length))
        - so - O(s1.length + s2.length + max(m1.length, m2.length)) = O(max(s1, s2, m1, m2)) = O(n)
        
        solution2: sort strings, compare them
        time:
        - sort both strings = O(s1 log s1 + s2 log s2)
        - compare = O(s.length)
        - so = O(s log s)
        
        optimizations:
        - if strings are different length -> false
    *)
    let isPermutationHash (str1 : string) (str2 : string) =
        let buildCharCountMap (str : string) =
            str
            |> Seq.toList
            |> Seq.fold (fun (sum : Map<char,int>) item ->
                match sum.TryGetValue item with
                | true, value -> sum |> Map.add item (value + 1)
                | false, _ -> sum |> Map.add item 1
            ) Map.empty<char,int>
                
        if str1.Length <> str2.Length then false
        else 
            let map1 = buildCharCountMap str1
            let map2 = buildCharCountMap str2
            
            if map1.Count <> map2.Count then false
            else
                let rec loop map1 (map2 : Map<char,int>) =
                    match map1 with
                    | [] -> true
                    | (k,v1)::tail ->
                        match map2.TryGetValue k with
                        | true, v2 -> if v1 = v2 then loop tail map2 else false
                        | false, _ -> false
                
                loop (map1 |> Map.toList) map2

    let isPermutationArray (str1 : string) (str2 : string) =
        if str1.Length <> str2.Length then false
        else
            let arr = Array.init 128 (fun _ -> 0)
            for c in str1 do
                let index = c |> int
                arr.[index] <- arr.[index] + 1 
            
            let rec loop (arr : int []) (str2 : char list) =
                match str2 with
                | [] -> true
                | head::tail ->
                    let index = head |> int
                    arr.[index] <- arr.[index] - 1
                    if arr.[index] < 0 then false
                    else loop arr tail
            loop arr (str2 |> Seq.toList)
                
    open System.Linq

    let isPermutationSort (str1 : string) (str2 : string) =
        if str1.Length <> str2.Length then false
        else
            let sorted1 = Array.sort (str1.ToCharArray())
            let sorted2 = Array.sort (str2.ToCharArray())
            sorted1.SequenceEqual(sorted2)
            
module Urification =
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


module PalindromePermutation =
    (*
    Given a string, write a function to check if it is permutation of palindrome.
    You can ignore casing and non-letter chars.
    
    For example:
    input: Tact Coa
    output: true -> taco cat, atco cta
    
    I probably need to iterate through chars anyway, so best runtime - O(str.length).
    Palindrome means:
    - all individual letter count is even OR
    - one individual letter count is uneven, but all others are even
    
    1- I can build a list with letter counts -> O(n)
    2- I can apply even/uneven logic at it -> O(list.length)
    3- Space -> O(list + str)
    
    To reduce space and not use list, i could maybe...
    1- Sort chars -> O(n log n)
    2- Iterate and look for repeating chars and count their pairs -> O(n)
        - this might get a bit tricky
    3- But i wouldn't need any additional space.
    *)
    
    let isPalindromePermutation (str : string) =
        // would swap this with a traditional for loop -> piping to save space 
        let counts =
            str
            |> Seq.filter Char.IsLetter
            |> Seq.map Char.ToLower
            |> Seq.countBy id
            |> Seq.map snd
            |> Seq.toList
            
        let odd = counts |> List.filter (fun v -> v % 2 = 0)
        
        odd.Length = 0 || odd.Length = 1
        
module OneAway =
    (*
    There are three types of edits that can be performed on a string: insert char,
    remove char or replace a char. Given two strings, write a function to check if they are
    one or zero edits away.
    
    Samples:
    pale,ple -> true
    pales, pale -> true
    pale, bale -> true
    pale, bake -> false
    
    Solution 1:
    - if str length diff is > 1, its false
    - if i iterate through string, insert and remove should negate each other, so i should be ok to use only insert
    - iterate longer (if length don't match) and use insert on shorter (could iterate shorter and use remove)
    - create flag to indicate edit was done
    - check each char is equal
    - if lengths don't match, i'll always need to do an insert in the shorter string (the only necessary edit)
    - if lengths match, i'll need to do a replace
    - this would work but logic and indexing is very ugly and i'll get confused
    - time - O(max(s1.length, s2.length))
    - space - O(1)
    
    Solution 2 (assuming ASCII strings):
    - if str length diff is > 1, its false
    - create array with char count for each string
    - compare char counts - max one char count shouldn't match to be true
    - otherwise - false
    - NO!! -> this won't work because character order is important and count will not consider that.
    - time - O(max(s1.length, s2.length))
    - space - O(s1 + s2) => O(n)
    
    *)
    
    let private isOneEditEqualLengths (str1 : string) (str2 : string) =
        let rec iterate edited index (str1 : char[]) (str2 : char[]) =
            if index = str1.Length then true
            else 
                match edited, str1.[index] = str2.[index] with
                | false, false -> iterate true (index + 1) str1 str2
                | true, false -> false
                | _ -> iterate edited (index + 1) str1 str2
        
        iterate false 0 (str1.ToCharArray()) (str2.ToCharArray())
        
    let private isOneEditDifferentLengths (str1 : string) (str2 : string) =
        let rec iterate edited longIndex shortIndex (longer : char[]) (shorter : char[]) =
            if shortIndex = shorter.Length then true
            else
                match edited, longer.[longIndex] = shorter.[shortIndex] with
                | false, false -> iterate true (longIndex + 1) shortIndex longer shorter
                | true, false -> false
                | _ -> iterate edited (longIndex + 1) (shortIndex + 1) longer shorter
        
        let (longer,shorter) = if str1.Length > str2.Length then (str1,str2) else (str2,str1)
        iterate false 0 0 (longer.ToCharArray()) (shorter.ToCharArray())
    
    let isOneEdit (str1 : string) (str2 : string) =
        let absoluteDiff = abs (str1.Length - str2.Length)
        
        match absoluteDiff with
        | 0 -> isOneEditEqualLengths str1 str2
        | 1 -> isOneEditDifferentLengths str1 str2
        | _ -> false                