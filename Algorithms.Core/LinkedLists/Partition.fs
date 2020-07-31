module Partition

open System
open LinkedList

(*
Write code to partition a linked list around value x, such that all nodes less than x come before all nodes
greater than or equal to x. Important: the partition element x can appear anywhere in the right partition; it
does not need to appear between the left and the right partitions. The additional spacing in the example below
indicates the partition. Yes, the output below is one of many valid outputs.

Example:
Input:        3 -> 5 -> 8 -> 5 -> 10 -> 2 -> 1 [ partition = 5 ]
Output:       3 -> 1 -> 2      ->      10 -> 5 -> 5 -> 8

Solution #1:
- I could iterate list, check if item is lower than partition and if it is
    - remove item from list (time - O(1))
    - move item to head of list (time - O(1))
- for example:
3 < 5 ? true -> node.value = node.next.value, node.next = node.next.next, list.push_front(node)
5 < 5 ? false -> move next
8 < 5 ? false -> move next
5 < 5 ? false -> move next
10 < 5 ? false-> move next
2 < 5 ? true -> node.value = node.next.value, node.next = node.next.next, list.push_front(node) (2 -> 3 -> .....)
1 < 5 ? true -> node.value = node.next.value, node.next = node.next.next, list.push_front(node) (1 -> 2 -> 3 -> .....)

Bad: handling two pointers is a little complicated.

Time: O(list.length + 1 + 1) -> O(n) (need to iterate list)
Space: O(1) (no additional needed)

Solution #2:
- From book - apparently optimal solution is to create two additional lists and move elements into them
as i iterate elements and then merge those two lists.

*)

let partitionList (list : SinglyLinkedList<int>) (partition : int) : SinglyLinkedList<int> =
    let rec iterate (list : SinglyLinkedList<int>) (previous : SinglyNode<int>) (node : SinglyNode<int> option) =
        match node with
        | None -> list
        | Some nodeValue -> 
            if nodeValue.Value < partition then
                match nodeValue.Next with
                | None ->
                    previous.Next <- None
                    list.AddFront nodeValue.Value
                    iterate list previous previous.Next
                    
                | Some nextValue ->
                    let memorized = nodeValue.Value
                    nodeValue.Value <- nextValue.Value
                    nodeValue.Next <- nextValue.Next
                    list.AddFront memorized
                    iterate list previous node
            else
                iterate list nodeValue nodeValue.Next
    
    if list.Head.IsNone then list
    else
        iterate list list.Head.Value list.Head.Value.Next