module Palindrome

open System.Collections.Generic
open LinkedList

(*
Write a function to check if a linked list is a palindrome. (its when list reads same backwards and forwards)

Notes:
- If this is a doubly linked list, i could use two pointers, head and tail, and iterate them towards each
other until they meet, and compare each item to make sure its valid. Time - O(n/2), Space - O(1)
- If list length is known, I could calculate middle node, iterate from it to end and add all items to a stack. Then
iterate from start removing each item from stack and comparing them. Time - O(2n), Space - O(n/2)
- I could iterate list, add pointers to a stack and a queue. When done, I would iterate both structures removing items
and comparing them until I get to the same item. Time - O(n + n/2), Space - O(2n)
- I could iterate list, add pointers to a stack only. When done, I iterate list again and remove items from stack +
compare them with items from list until I run into the same item. Time - O(n + n/2), Space - O(n)
- I could improve the above by using slow/fast pointers to jump the list and add only second half of items to the stack.
Then I could compare items in stack to first half of items in list. Time - O(n + n/2), Space - O(n/2) 
*)

let isPalindrome (list : SinglyLinkedList<int>) : bool =
    let buildStack (list : SinglyLinkedList<int>) =
        let rec loop (node : SinglyNode<int> option) (stack : Stack<SinglyNode<int>>) =
            match node with
            | None -> stack
            | Some x ->
                stack.Push x
                loop x.Next stack
        loop list.Head (Stack<SinglyNode<int>>())
    let tryPop (stack : Stack<_>) =
        match stack.TryPop() with
        | true, x -> Some x
        | _ -> None
    
    if list.Head.IsNone then true
    else 
        let stack = buildStack list
        let stopMark = if stack.Count % 2 = 0 then stack.Count / 2 else (stack.Count / 2) + 1
        let rec iterate (currentIndex : int) (listNode : SinglyNode<int> option) (stackNode : SinglyNode<int> option) =
            match listNode, stackNode with
            | Some(x), Some(y) when currentIndex = stopMark && x.Value = y.Value -> true
            | Some(x), Some(y) ->
                if x.Value = y.Value then iterate (currentIndex + 1) x.Next (tryPop stack)
                else false
            | _ -> false
            
        iterate 1 list.Head (tryPop stack)