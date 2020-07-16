#load "LinkedLists/LinkedList.fs"
#load "LinkedLists/DuplicateRemoval.fs"

open DuplicateRemoval
open LinkedList

let list = LinkedList.SinglyLinkedList<int>()
list.AddFront 1
list.AddFront 2
list.AddFront 3
list.AddFront 2
list.AddFront 1
list.AddFront 1
list.AddFront 1
list.AddFront 1
list.AddFront 1
list.AddFront 1

list.List() |> printfn "%A"
