module Partition

open LinkedList
open Xunit

[<Fact>]
let ``Given list, When empty, Then return empty``() =
    let list = SinglyLinkedList<int>()
    let partition = 5
    let result = Partition.partitionList list partition |> fun l -> l.List() |> List.toSeq
    
    Assert.Empty result

[<Fact>]
let ``Given list, When one element, Then return self``() =
    let list = SinglyLinkedList<int>()
    list.AddFront 1
    
    let partition = 5
    let result = Partition.partitionList list partition |> fun l -> l.List() |> List.toSeq
    
    Assert.Equal([1], result) 

[<Fact>]
let ``Given list, When multiple elements, Then return partitioned``() =
    let list = SinglyLinkedList<int>()
    list.AddFront 1
    list.AddFront 2
    list.AddFront 10
    list.AddFront 5
    list.AddFront 8
    list.AddFront 5
    list.AddFront 3
    
    let partition = 5
    let result = Partition.partitionList list partition |> fun l -> l.List() |> List.toSeq
    
    Assert.Equal([1;2;3;5;8;5;10], result) 
