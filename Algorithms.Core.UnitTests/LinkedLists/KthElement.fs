module KthElement

open LinkedList
open Xunit

[<Fact>]
let ``Given list, When empty, Then return none``() =
    let list = SinglyLinkedList()
    let result = KthElement.findKthLastOptimal list 4
    Assert.Equal(None, result)
    
[<Fact>]
let ``Given list, When k greater than length, Then return none``() =
    let list = SinglyLinkedList()
    list.AddFront 1
    
    let result = KthElement.findKthLastOptimal list 4
    Assert.Equal(None, result)
    
[<Fact>]
let ``Given list, When k zero, Then return none``() =
    let list = SinglyLinkedList()
    list.AddFront 3
    list.AddFront 2
    list.AddFront 1
    
    let result = KthElement.findKthLastOptimal list 0
    Assert.Equal(None, result)
    
[<Fact>]
let ``Given list, When k = LL.length, Then return head item``() =
    let list = SinglyLinkedList()
    list.AddFront 3
    list.AddFront 2
    list.AddFront 1
    
    let result = KthElement.findKthLastOptimal list 3
    Assert.Equal(Some(1), result)
    
[<Fact>]
let ``Given list, When k is in middle, Then return valid``() =
    let list = SinglyLinkedList()
    list.AddFront 5
    list.AddFront 4
    list.AddFront 3
    list.AddFront 2
    list.AddFront 1
    
    let result = KthElement.findKthLastOptimal list 3
    Assert.Equal(Some(3), result)