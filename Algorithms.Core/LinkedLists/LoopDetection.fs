module LoopDetection

open System.Collections.Generic
open LinkedList

(*
Given a linked list which might contain a loop, return a node which starts the loop.
Sample: A - B - C - D - E - C (same as before)
Output: C

Notes:
- I could iterate list, store them in a hashset, and check if each node's next node exist in set. If it does - return.
Time - O(N * 1), Space - O(N)
- The optimal solution:
    - if i have a slow/fast pointers, and iterate, they will catch up eventually - this will indicate loop
    - once the pointers meet, due to some crazy assumptions, they'll be at a point which is:
        - k ticks away from loop start
        - head of list is k ticks away from loop start
    - so i should move one of pointers to list head and iterate both pointers at same pace until they meet
    - meeting point is the node i was looking for.
    - Time - O(N), Space - O(1)

*)

let findLoop (list : SinglyLinkedList<'a>) : 'a option =
    if list.Head.IsNone then None
    else
        let rec loop (node : SinglyNode<'a> option) (set : HashSet<'a>) =
            match node with
            | None -> None
            | Some n ->
                if set.Contains n.Value then Some n.Value
                else
                    set.Add n.Value |> ignore
                    loop n.Next set
        
        loop list.Head (HashSet<'a>())
