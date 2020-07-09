module Uniqueness

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


