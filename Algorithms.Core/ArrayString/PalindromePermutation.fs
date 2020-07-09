module PalindromePermutation

open System

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
