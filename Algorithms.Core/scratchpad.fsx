#load "ArrayString.fs"

open ArrayString
open OneAway

(*
    pale,ple -> true
    pales, pale -> true
    pale, bale -> true
    pale, bake -> false
*)

let input1 = "pale"
let input2 = "ple"

isOneEdit input1 input2 
