module ZeroMatrix

(*
Write an algorithm such that if an element in an MxN matrix is 0, its entire row and column are set to 0.
Time - O(N*M) -> best, have to touch each element
Space - ??

        [1 2 3]
        [1 2 3]
        [1 0 3]
        [1 2 3]
        
        [1 0 3]
        [1 0 3]
        [0 0 0]
        [1 0 3]

Notes:
- Need to memorize which items are originally 0 so I don't accidentally set too many 0s


Solution #1:
- I could iterate the whole matrix and find indexes for 0
- Then calculate rows/columns to set to 0, and set them
- Time - O(N*M + k) where k is zero setting
- Space - O(N*M) if whole matrix is 0s

Solution #2:
- I could iterate matrix and change as I go
- As soon as i encounter 0, i'll
    - memorize location of zero 
    - set current row/col to zeroes
    - while doing that, i need to find any new zeroes
    - if new zeroes are encountered, i need to memorize their location and set their rows/cols to zeroes (recurse)
- Time - O(N*M + k) were k is zero setting
- Space - (N*M) if whole matrix is 0s
- Bad - this is much more complicated

Solution #3:
- I could iterate matrix and change as I go only items i already iterated
- As soon as i encounter 0, i'll
    - memorize location of zero 
    - set current row/col to zeroes only backwards (leave new items alone for now)
    - when i finish iterating, check memorized items and iterate remaining bits (forward)
- Time - O(N*M + k) were k is zero setting
- Space - (N*M) if whole matrix is 0s
- Bad - more complicated than #1 and potentially not saving anything.
*)

let setEmptyToZero (matrix : int[,]) : int[,] =
    let rows = matrix |> Array2D.length1
    let cols = matrix |> Array2D.length2
    let zeroIndexes = ResizeArray<_>()
    
    // find indexes
    for i in 0..rows-1 do
        for j in 0..cols-1 do
            if matrix.[i,j] = 0 then
                zeroIndexes.Add ((i,j))
    
    // replace  
    for (x,y) in zeroIndexes do
        
        for i in 0..rows-1 do
            matrix.[i,y] <- 0
            
        for j in 0..cols-1 do
            matrix.[x,j] <- 0
    
    matrix

let setEmptyZeroEfficient (matrix : int[,]) : int[,] =
    let rows = matrix |> Array2D.length1
    let cols = matrix |> Array2D.length2
    
    let zeroRows = Array.create rows 0
    let zeroCols = Array.create cols 0
    
    for i in 0..rows-1 do
        for j in 0..cols-1 do
            if matrix.[i,j] = 0 then
                zeroRows.[i] <- 1
                zeroCols.[j] <- 1
    
    for x in 0..rows - 1 do
        if zeroRows.[x] = 1 then
            for i in 0..cols-1 do
                matrix.[x,i] <- 0
    for y in 0..cols - 1 do
        if zeroCols.[y] = 1 then
            for i in 0..rows-1 do
                matrix.[i,y] <- 0

    matrix