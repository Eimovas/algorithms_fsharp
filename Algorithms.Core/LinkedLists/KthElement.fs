module KthElement

open LinkedList

(*
Implement an algorithm to find the kth to last element of a singly linked list.
Sample:
           6   5   4   3   2   1
List   - [ 4 - 3 - 7 - 4 - 3 - 1 ]
K      - 3
Result - 4

Notes :
- Assuming I don't have length of a linked list, which means i'll need to iterate
- If I had length, I would calculate index of item by: index = length - k = (6 - 3) = 3
- Without length:
    - Option 1:
        - Iterate list to end, assign index to each item in a map
        - As soon as i reach end, find item by calculating index = length - k
        - Lookup that item and return
        - Worst time - O(n)
        - Space - O(n) - will store a copy of list in a map
    - Option 2:
        - Iterate list to end to find length
        - When length is known, calculate item index = length - k
        - Iterate list again to index and return item
        - Worst time - O(2n) -> O(n) (iterating list twice)
        - Space - O(1) - not storing any copies
    - Option 3 (optimal):
        - create 2 pointers, p1 and p2, and place p2 at the start and p1 at start + (k - 1) nodes
        - if k = 3, then
          p2      p1    
          6   5   4   3   2   1
        [ 4 - 3 - 7 - 4 - 3 - 1 ]
        - iterate list and move both pointers ahead until p1 is null
        - when p1 is null, p2 will be the result
        - Time - O(n), Space - O(1)
*)

let rec private findLength (node : SinglyNode<int> option) index =
    match node with
    | None -> index
    | Some n -> findLength n.Next (index + 1)

let rec private getKthValueNaive (node : SinglyNode<int> option) currentIndex targetIndex =
    match node with
    | None -> None
    | Some n ->
        if currentIndex = targetIndex then Some n.Value
        else getKthValueNaive n.Next (currentIndex + 1) targetIndex

let findKthLastNaive (list : SinglyLinkedList<int>) (k : int) : int option =
    if list.Head.IsNone then None
    else
        let length = findLength list.Head 0
        let targetIndex = length - k
        getKthValueNaive list.Head 0 targetIndex
        
let rec private getKthNode (node : SinglyNode<int> option) currentIndex targetIndex =
    match node with
    | None -> None
    | Some n ->
        if currentIndex = targetIndex then node
        else getKthNode n.Next (currentIndex + 1) targetIndex
        
let rec private getKthValue (p1 : SinglyNode<int> option) (p2 : SinglyNode<int> option) (currentValue : int) =
    match p1,p2 with
    | None, None -> None
    | None, Some x -> Some currentValue
    | Some n1, Some n2 -> getKthValue n1.Next n2.Next n2.Value
    | _ -> None
        
        
let findKthLastOptimal(list : SinglyLinkedList<int>) (k : int) : int option =
    if list.Head.IsNone || k = 0 then None
    else
        let p2 = list.Head
        let p1 = getKthNode list.Head 0 (k - 1)
        
        if p1.IsNone then None
        elif p1 = p2 then Some p1.Value.Value
        else
            getKthValue p1 p2 p2.Value.Value
