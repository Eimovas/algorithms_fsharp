
let input = array2D [
    [1;2;3;4]
    [1;2;3;4]
    [1;2;3;4]
]
let expected = array2D [
    [1;1;1;1]
    [2;2;2;2]
    [3;3;3;3]
    [4;4;4;4]
] 

input |> Array2D.length1 

input.[0,*] <- Array.zeroCreate 4