module OneEditAway

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
