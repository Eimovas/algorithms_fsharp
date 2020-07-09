module Encoding

open Xunit

[<Fact>]
let ``Given string, When empty, Then return empty`` () =
    let input = System.String.Empty
    Assert.Equal (System.String.Empty, Encoding.encode input)

[<Fact>]
let ``Given string, When single char, Then return single char`` () =
    let input = "a"
    Assert.Equal ("a", Encoding.encode input)
    
[<Theory>]
[<InlineData("abcde", "abcde")>]
[<InlineData("aabbccddee", "aabbccddee")>]
[<InlineData("aabbccddeeee", "a2b2c2d2e4")>]
[<InlineData("aaaaaaC", "a6C1")>]
let ``Given string, When encoding, Then return valid`` input expected =
    Assert.Equal (expected, Encoding.encode input)