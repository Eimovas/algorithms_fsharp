module DuplicateRemoval

open System.Collections.Generic
open LinkedList

(*
Write code to remove duplicates from an unsorted linked list.
Follow up - how would you solve this problem if temporary buffer is not allowed?

Sample: 1-2-2-1-3-4-5-4-3-2
Result: 1-2-3-4-5

Notes:

Solution 1.
- I could iterate list and add each item to hashset and at the same time -> use doubly linked list to add unique items to back
- By using doubly linked list i would maintain order of items
- Time -> O(n) - iterate original list, hashset checks are O(1) and doubly list inserts to back are O(1)
- Space -> O(2n) - because worst case, hash could contain all items in list and i'll create a new linked list

Solution 2.
I'm not allowed to use buffer...
- I'd have to take memorize first item and iterate list and remove duplicates of it
- then i'd memorize second item, and iterate list and remove duplicates of it...
- Time -> O(n*n)
- Space - O(1)
*)

let removeDuplicates (list : SinglyLinkedList<int>) =
    let buffer = HashSet<int>()
    
    let rec loop (list : SinglyLinkedList<int>) (current : SinglyNode<int> option) (previous : SinglyNode<int> option) =
        match current with
        | None -> list
        | Some currentValue when buffer.Contains currentValue.Value -> 
            previous.Value.Next <- currentValue.Next
            loop list currentValue.Next previous
        | Some currentValue ->
            buffer.Add(currentValue.Value) |> ignore
            loop list currentValue.Next current
    
    loop list list.Head None
