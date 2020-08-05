module Palindrome

open LinkedList
open Xunit

[<Fact>]
let ``Given list, When empty, Return true``() =
    let list = SinglyLinkedList<int>()
    Assert.True(Palindrome.isPalindrome list)
    
[<Fact>]
let ``Given list, When one item, Return true``() =
    let list = SinglyLinkedList<int>()
    list.AddFront 1
    
    Assert.True(Palindrome.isPalindrome list)

[<Fact>]
let ``Given list, When not palindrome, Return false``() =
    let list = SinglyLinkedList<int>()
    list.AddFront 1
    list.AddFront 2
    
    Assert.False(Palindrome.isPalindrome list)
    
[<Fact>]
let ``Given list, When palindrome with even count, Return true``() =
    let list = SinglyLinkedList<int>()
    list.AddFront 1
    list.AddFront 2
    list.AddFront 2
    list.AddFront 1
    
    Assert.True(Palindrome.isPalindrome list)
    
[<Fact>]
let ``Given list, When palindrome with odd count, Return true``() =
    let list = SinglyLinkedList<int>()
    list.AddFront 1
    list.AddFront 2
    list.AddFront 1
    
    Assert.True(Palindrome.isPalindrome list)
