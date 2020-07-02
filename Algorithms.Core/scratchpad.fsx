#load "ArrayString.fs"

open ArrayString
open Urification

let input1 =  "i am going for a walk                  "
let result1 = "i%20am%20going%20for%20a%20walk"

encodeArrayCorrect input1 21 |> printfn "%s"

for i in 10..-1..0 do
    printfn "%i" i