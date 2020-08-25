module Intersection

open LinkedList
open Xunit

[<Fact>]
let ``Given two lists, When first empty, Then return none`` () =
    let list1 = SinglyLinkedList<_>()
    let list2 = SinglyLinkedList<_>()
    list2.AddFront (box 2)
    
    let result = Intersection.getIntersection list1 list2
    
    Assert.True(result.IsNone)

[<Fact>]
let ``Given two lists, When second empty, Then return none`` () =
    let list1 = SinglyLinkedList<_>()
    let list2 = SinglyLinkedList<_>()
    list1.AddFront (box 2)
    
    let result = Intersection.getIntersection list1 list2
    
    Assert.True(result.IsNone)
(*
Sample:
4 - 3 - 6
         \
          4 - 5- 9
         /
    2 - 5
*)
[<Fact>]
let ``Given two lists, When intersecting, Then return valid`` () =
    let list1 = SinglyLinkedList<_>()
    let list2 = SinglyLinkedList<_>()
    let intersecting1 = box 9
    let intersecting2 = box 5
    let intersecting3 = box 4
    
    list1.AddFront intersecting1
    list1.AddFront intersecting2
    list1.AddFront intersecting3
    list2.AddFront intersecting1
    list2.AddFront intersecting2
    list2.AddFront intersecting3
    
    list1.AddFront (box 6)
    list1.AddFront (box 3)
    list1.AddFront (box 4)
    list2.AddFront (box 5)
    list2.AddFront (box 2)
    
    let result = Intersection.getIntersection list1 list2
    
    Assert.True(result.IsSome)
    Assert.Equal(Some(intersecting3), result)
    
[<Fact>]
let ``Given two lists, When not intersecting, Then return none`` () =
    let list1 = SinglyLinkedList<_>()
    let list2 = SinglyLinkedList<_>()
    list1.AddFront (box 2)
    list1.AddFront (box 1)
    list2.AddFront (box 1)
    list2.AddFront (box 2)
    
    let result = Intersection.getIntersection list1 list2
    
    Assert.True(result.IsNone)
    