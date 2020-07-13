module MatrixRotation

open Xunit

[<Fact>]
let ``Given matrix, When empty, Then return empty``() =
    let empty = array2D []
    let result = MatrixRotation.rotate empty
    Assert.Equal (empty, result)

[<Fact>]
let ``Given matrix, When small, Then return rotated``() =
    let input = array2D [
        [1;2]
        [1;2]
    ]
    let expected = array2D [
        [1;1]
        [2;2]
    ]
    let result = MatrixRotation.rotate input
    Assert.Equal (expected, result)

[<Fact>]
let ``Given matrix, When large, Then return rotated``() =
    let input = array2D [
        [1;2;3;4]
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
    let result = MatrixRotation.rotate input
    Assert.Equal (expected, result)
