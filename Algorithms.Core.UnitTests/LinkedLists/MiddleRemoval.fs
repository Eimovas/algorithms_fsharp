module MiddleRemoval

open LinkedList
open Xunit

[<Fact>]
let ``Given list with one item, When node not matching, Then return list``() =
    let list = SinglyLinkedList<int>()
    list.AddFront 1
    
    let result = MiddleRemoval.removeMiddle list 2 |> fun l -> l.List() |> List.toSeq
    Assert.Equal([1], result)
    
[<Fact>]
let ``Given list with one item, When node matching, Then return empty list``() =
    let list = SinglyLinkedList<int>()
    list.AddFront 1
    
    let result = MiddleRemoval.removeMiddle list 1 |> fun l -> l.List() |> List.toSeq
    Assert.Equal([], result) 
    
[<Fact>]
let ``Given list, When node matching, Then return valid list``() =
    let list = SinglyLinkedList<int>()
    list.AddFront 1
    list.AddFront 2
    list.AddFront 3
    list.AddFront 4
    
    let result = MiddleRemoval.removeMiddle list 2 |> fun l -> l.List() |> List.toSeq
    Assert.Equal([4;3;1], result) 