module SumLists

open LinkedList
open Xunit

[<Fact>]
let ``Given lists, When first is empty, Then return valid``() =
    let first = SinglyLinkedList<char>()
    let second = SinglyLinkedList<char>()
    second.AddFront '1'
    second.AddFront '2'
    
    let result = SumLists.addListsNaive first second |> fun l -> l.List() |> Seq.ofList
    Assert.Equal(['2';'1'], result)
    
[<Fact>]
let ``Given lists, When second is empty, Then return valid``() =
    let first = SinglyLinkedList<char>()
    let second = SinglyLinkedList<char>()
    first.AddFront '1'
    first.AddFront '2'
    
    let result = SumLists.addListsNaive first second |> fun l -> l.List() |> Seq.ofList
    Assert.Equal(['2';'1'], result)
    
[<Fact>]
let ``Given lists, When both empty, Then return empty``() =
    let first = SinglyLinkedList<char>()
    let second = SinglyLinkedList<char>()
    
    let result = SumLists.addListsNaive first second |> fun l -> l.List() |> Seq.ofList
    Assert.Equal([], result)
    
[<Fact>]
let ``Given lists, When non empty, Then return valid``() =
    let first = SinglyLinkedList<char>()
    first.AddFront '6'
    first.AddFront '1'
    first.AddFront '7'
    
    let second = SinglyLinkedList<char>()
    second.AddFront '2'
    second.AddFront '9'
    second.AddFront '5'
    
    let result = SumLists.addListsNaive first second |> fun l -> l.List() |> Seq.ofList
    Assert.Equal(['2';'1';'9'], result)