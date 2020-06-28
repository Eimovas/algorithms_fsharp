#load "ArrayString.fs"

open ArrayString

let unique = "very"
let notUnique = "i am not unique"

hasUniqueCharsHash unique |> printfn "%b"
hasUniqueCharsHash notUnique |> printfn "%b"

