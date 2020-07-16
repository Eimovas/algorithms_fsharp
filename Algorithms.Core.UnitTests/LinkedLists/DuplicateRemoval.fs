module DuplicateRemoval

open Xunit

[<Fact>]
let ``Given list, When no duplicates, Then return original``() =
    let input = LinkedList.SinglyLinkedList<int>()
    input.AddFront 1
    input.AddFront 2
    input.AddFront 3
    
    let expected = [3;2;1]
    
    let result = DuplicateRemoval.removeDuplicates input |> fun l -> l.List() |> List.toSeq
    
    Assert.Equal (expected,result)

[<Fact>]
let ``Given list, When contain duplicates, Then return without duplicates``() =
    let input = LinkedList.SinglyLinkedList<int>()
    input.AddFront 1
    input.AddFront 2
    input.AddFront 2
    input.AddFront 3
    input.AddFront 3
    input.AddFront 3
    input.AddFront 1
    
    let expected = [1;3;2]
    
    let result = DuplicateRemoval.removeDuplicates input |> fun l -> l.List() |> List.toSeq
    
    Assert.Equal (expected,result)

