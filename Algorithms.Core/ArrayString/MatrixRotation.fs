module MatrixRotation

// Given an image represented by an NxN matrix, where each pixel in the image is represented by an integer
// write a method to rotate the image 90 degrees. Can you do this in place?

(*
    [
        [ 1 2 3 4 ]
        [ 1 2 3 4 ]
        [ 1 2 3 4 ]
        [ 1 2 3 4 ]
    ]
    
    [
        [ 1 1 1 1 ]
        [ 2 2 2 2 ]
        [ 3 3 3 3 ]
        [ 4 4 4 4 ]
    ]
    
    Note:
    - I could take column items as array, and build a new matrix with that column being row. That would need a new array
    - I could move pointer in diagonal location and swap items in place making starting index at diagonal location.
        - start pointer at 0,0
        - swap 0,1 with 1,0
        - swap 0,2 with 2,0
        - swap 0,3 with 3,0
        - move pointer to 1,1
        - swap 1,2 with 2,1
        - swap 1,3 with 3,1
        - move pointer to 2,2
        - swap 2,3 with 3,2
        - move pointer to 3,3
        - finish
    
    - Solution - iterate diagonal and do swaps in place
    - Space -> O(1)
    - Time -> O (N * N) -> have to touch each item, and matrix is N*N
*)

let rotate (matrix : int[,]) : int[,] =
    let size = matrix.GetLength 0
    for i in 0..size - 1 do
        for j in (i+1)..size - 1 do
            let row = matrix.[i,j]
            let col = matrix.[j,i]
            matrix.[i,j] <- col
            matrix.[j,i] <- row
    matrix
    
