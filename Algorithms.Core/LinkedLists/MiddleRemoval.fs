module MiddleRemoval

open LinkedList

(*
Implement an algorithm to delete a node in the middle of a singly linked list, given only access to that node.

Questions:
- when you say 'given access to only that node' - does that mean i'm unable to access other nodes or iterate the list?
    - i hope not
- what should happen if node is not found in list? I'll assume i'll return unchanged list
    
Solution #1:
- have pointer p1 and p2, iterate list with p1 at current index and p2 at index-1
- if p1 == given node, then p2.next = p1.next
- stop loop
- Time - O(n) - worst case i'll iterate the whole list
- Space - O(1)

Solution #2 (correct)
- I only have access to the target node (to its reference)
- I can take value from next node, use it for current node, and update my next pointer to be next node's next
- This won't work if node to remove is last
- Time - O(1), Space - O(1)
*)

let rec private removeNode (p1 : SinglyNode<int> option) (p2 : SinglyNode<int> option) (target : int) =
    match p1, p2 with
    | Some x, Some y ->
        if x.Value = target then
            y.Next <- x.Next
            removeNode None None target
        else removeNode x.Next y.Next target
    | _ ->  None
        

let removeMiddle (list : SinglyLinkedList<int>) (node : int) =
    match list.Head with
    | None -> list
    | Some n -> 
        let p1 = n.Next
        let p2 = list.Head
        
        if p1.IsNone && p2.Value.Value = node then SinglyLinkedList<int>()
        else
            let _ = removeNode p1 p2 node        
            list

let removeMiddleOptimised (list : SinglyLinkedList<int>) (node : SinglyNode<int>) =
    match node.Next with
    | None -> failwith "can't solve this if given node is last in list"
    | Some n ->
        node.Value <- n.Value
        node.Next <- n.Next
        list