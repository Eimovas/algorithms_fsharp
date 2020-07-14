module ZeroMatrix

open Xunit

[<Fact>]
let ``Given matrix, When empty, Then return empty``() =
    let input = array2D []
    Assert.Equal(array2D [], ZeroMatrix.setEmptyToZero input)
    Assert.Equal(array2D [], ZeroMatrix.setEmptyZeroEfficient input)
    
[<Fact>]
let ``Given matrix, When N > M, Then return valid``() =
    let input = array2D [
        [1;2;3;4]
        [1;2;3;0]
        [1;2;3;4]
    ]

    let expected = array2D [
        [1;2;3;0]
        [0;0;0;0]
        [1;2;3;0]
    ]
    
    Assert.Equal(expected, ZeroMatrix.setEmptyZeroEfficient input)

[<Fact>]
let ``Given matrix, When M > N, Then return valid``() =
    let input = array2D [
        [1;2;3]
        [1;2;3]
        [1;0;3]
        [1;2;3]
    ]
    let expected = array2D [
        [1;0;3]
        [1;0;3]
        [0;0;0]
        [1;0;3]
    ]
    
    Assert.Equal(expected, ZeroMatrix.setEmptyZeroEfficient input)

[<Fact>]
let ``Given matrix, When M == N, Then return valid``() =
    let input = array2D [
        [1;2;3;4]
        [1;2;3;4]
        [1;0;3;4]
        [1;2;3;4]
    ]
    let expected = array2D [
        [1;0;3;4]
        [1;0;3;4]
        [0;0;0;0]
        [1;0;3;4]
    ]
    
    Assert.Equal(expected, ZeroMatrix.setEmptyZeroEfficient input)

[<Fact>]
let ``Given matrix, When multiple zeroes, Then return valid``() =
    let input = array2D [
        [1;2;3;4]
        [1;2;3;4]
        [1;0;3;4]
        [1;2;3;4]
        [1;2;3;0]
    ]
    let expected = array2D [
        [1;0;3;0]
        [1;0;3;0]
        [0;0;0;0]
        [1;0;3;0]
        [0;0;0;0]
    ]
    
    Assert.Equal(expected, ZeroMatrix.setEmptyZeroEfficient input)

[<Fact>]
let ``Given matrix, When multiple zeroes in same row, Then return valid``() =
    let input = array2D [
        [1;2;3;4]
        [1;2;3;4]
        [1;0;3;0]
        [1;2;3;4]
        [1;2;3;4]
    ]
    let expected = array2D [
        [1;0;3;0]
        [1;0;3;0]
        [0;0;0;0]
        [1;0;3;0]
        [1;0;3;0]
    ]
    
    Assert.Equal(expected, ZeroMatrix.setEmptyZeroEfficient input)

[<Fact>]
let ``Given matrix, When multiple zeroes in same column, Then return valid``() =
    let input = array2D [
        [1;2;3;4]
        [1;2;3;4]
        [1;0;3;4]
        [1;0;3;4]
        [1;2;3;4]
    ]
    let expected = array2D [
        [1;0;3;4]
        [1;0;3;4]
        [0;0;0;0]
        [0;0;0;0]
        [1;0;3;4]
    ]
    
    Assert.Equal(expected, ZeroMatrix.setEmptyZeroEfficient input)

[<Fact>]
let ``Given matrix, When all zeroes, Then return valid``() =
    let input = array2D [
        [0;0;0;0]
        [0;0;0;0]
        [0;0;0;0]
        [0;0;0;0]
        [0;0;0;0]
    ]
    let expected = array2D [
        [0;0;0;0]
        [0;0;0;0]
        [0;0;0;0]
        [0;0;0;0]
        [0;0;0;0]
    ]
    
    Assert.Equal(expected, ZeroMatrix.setEmptyZeroEfficient input)
[<Fact>]
let ``Given matrix, When all non-zeroes, Then return valid``() =
    let input = array2D [
        [1;1;1;1]
        [1;1;1;1]
        [1;1;1;1]
        [1;1;1;1]
        [1;1;1;1]
    ]
    let expected = array2D [
        [1;1;1;1]
        [1;1;1;1]
        [1;1;1;1]
        [1;1;1;1]
        [1;1;1;1]
    ]
    
    Assert.Equal(expected, ZeroMatrix.setEmptyZeroEfficient input)

