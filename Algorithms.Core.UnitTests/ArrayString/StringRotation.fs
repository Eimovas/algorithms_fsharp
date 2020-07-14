module StringRotation

open Xunit

[<Theory>]
[<InlineData("erbottlewat", "waterbottle")>]
[<InlineData("waterbottle", "waterbottle")>]
[<InlineData("bottlewater", "waterbottle")>]
let ``Given two strings, When valid rotation, Then return true``(str1 : string, str2 : string) =
    let result = StringRotation.isRotation str1 str2
    Assert.True result
    
[<Theory>]
[<InlineData("erbottlewatt", "waterbottle")>]
[<InlineData("erbottlewa", "waterbottle")>]
[<InlineData("erbottlewat", "waterbottlee")>]
let ``Given two strings, When NOT valid rotation, Then return false``(str1 : string, str2 : string) =
    let result = StringRotation.isRotation str1 str2
    Assert.False result