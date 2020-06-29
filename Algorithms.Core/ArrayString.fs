module ArrayString

open System

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