module Encoding

open System.Text

let encode (str : string) =
    if str.Length = 0 then str
    else
        let mutable previousChar = str.[0]
        let mutable currentCount = 0
        let sb = StringBuilder()
        
        for c in str do
            if c = previousChar then
                currentCount <- currentCount + 1
            else
                sb.Append previousChar |> ignore      
                sb.Append currentCount |> ignore
                previousChar <- c
                currentCount <- 1
        
        sb.Append previousChar |> ignore        
        sb.Append currentCount |> ignore
        
        if sb.Length >= str.Length then str
        else sb.ToString()