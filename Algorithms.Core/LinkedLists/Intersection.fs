module Intersection

open LinkedList

(*
Given two singly linked lists, determine if the two lists intersect. Return the intersected node.
Note that the intersection is determined based on reference, not value. That is, if the kth node of the first linked
list is the same node as the jth node of the second linked list, then they are intersecting.

Sample:
4 - 3 - 6
         \
          4 - 5- 9
         /
    2 - 5

Questions:
- What if lists intersect at multiple points?

Notes:
- [bad] I could iterate first list, and for each node, iterate second list and check if they're the same. If they are,
return the node and mark lists as intersecting. Time - O(NxM) (N,M - lengths of lists), Space - O(1)
- [bad] I could iterate and store first list in hashset, then iterate second list and look for matching nodes in the set.
If any nodes match - return node and mark as intersecting. Time - O(N + (Mx1)), Space - O(N) 
- [good] Since intersecting lists will always end with the same element (if they share a node, next pointer on that
node will lead to the same node on both lists thereafter). Which means, the solution:
    - find lengths of lists
    - get diff
    - iterate both lists, make sure in longer list you start at the diff
    - compare both nodes and return matching

*)

let private isSameObject = LanguagePrimitives.PhysicalEquality
let private getListLength (list : SinglyLinkedList<'a>) : int =
    let rec loop (node : SinglyNode<'a> option) count =
        match node with
        | None -> count
        | Some n -> loop n.Next (count + 1)
    loop list.Head 0

let rec private getIntersectingNode (node1 : SinglyNode<'a> option) (node2 : SinglyNode<'a> option) : SinglyNode<'a> option =
    match node1, node2 with
    | Some x, Some y ->
        if isSameObject x.Value y.Value then node1
        else getIntersectingNode x.Next y.Next
    | _ -> None

let rec private getStartingNode (node : SinglyNode<'a> option) currentIndex startIndex : SinglyNode<'a> option =
    match node with
    | None -> None
    | Some n ->
        if currentIndex = startIndex then Some n
        else
            getStartingNode n.Next (currentIndex + 1) startIndex

let getIntersection (list1 : SinglyLinkedList<'a>) (list2 : SinglyLinkedList<'a>) : 'a option =
    if list1.Head.IsNone || list2.Head.IsNone then None
    else 
        let length1 = getListLength list1
        let length2 = getListLength list2
        let diff = abs (length1 - length2)
        
        let result = 
            if diff = 0 then
                getIntersectingNode list1.Head list2.Head
            elif length1 > length2 then
                let startNode = getStartingNode list1.Head 0 diff
                getIntersectingNode startNode list2.Head
            else
                let startNode = getStartingNode list2.Head 0 diff
                getIntersectingNode list1.Head startNode
                
        match result with
        | None -> None
        | Some x -> Some x.Value
