module LoopDetection

open LinkedList
open Xunit

[<Fact>]
let ``Given list, When empty, Then return none``() =
    let list = SinglyLinkedList<_>()
    let result = LoopDetection.findLoop list
    Assert.True result.IsNone
    
[<Fact>]
let ``Given list, When no loop, Then return none``() =
    let list = SinglyLinkedList<_>()
    list.AddFront (box 1)
    list.AddFront (box 2)
    list.AddFront (box 3)
    
    let result = LoopDetection.findLoop list
    Assert.True result.IsNone
    
[<Fact>]
let ``Given list, When has loop, Then return loop start value``() =
    let isSameObject = LanguagePrimitives.PhysicalEquality
    let list = SinglyLinkedList<_>()
    let loopNode = box 4
    
    list.AddFront loopNode
    list.AddFront (box 3)
    list.AddFront (box 2)
    list.AddFront loopNode
    list.AddFront (box 1)
    
    let result = LoopDetection.findLoop list
    Assert.True result.IsSome
    Assert.True(isSameObject result.Value loopNode)
    