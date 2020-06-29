#load "ArrayString.fs"

open ArrayString
open Permutation

let str1 = "jonas"
let str2 = "sanoj"

let str3 = "one"
let str4 = "two"

isPermutationArray str1 str2 |> printfn "%b"
