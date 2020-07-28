module LinkedList

type SinglyNode<'a> = {
    mutable Value : 'a
    mutable Next : SinglyNode<'a> option
}

(*
For Singly Linked List
- Add item to front - O(1) -> just update reference
- Add item to back - O(n) -> have to find last current item and update next reference
- Remove head - O(1) -> just update head next reference to be the new head
- Remove tail - O(n) -> have to find last current item and update references
- Add/remove item in middle - O(n) -> have to iterate and find where to add/remove
*)    
type SinglyLinkedList<'a when 'a : equality>() =
    let mutable head : SinglyNode<'a> option = None
    
    let add item =
        match head with
        | None -> head <- Some { Value = item; Next = None }
        | Some h ->
            let node = { Value = item; Next = Some h }
            head <- Some node
    
    let rec iterate head result =
        match head with
        | Some v -> iterate v.Next (v.Value::result)
        | None -> List.rev result
    
    member _.Head with get() = head
    member _.AddFront item = add item
    member _.List() = iterate head List.empty
    
    
type private DoublyNode<'a> = {
    Value : 'a
    Next : DoublyNode<'a> option
    mutable Prev : DoublyNode<'a> option
}
 

(*
For Doubly Linked List
- Add item to front - O(1) -> just update reference
- Add item to back - O(1) -> just update tail references
- Remove head - O(1) -> just update head next reference to be the new head
- Remove tail - O(1) -> just update tail references
- Add/remove item in middle - O(n) -> have to iterate and find where to add/remove
*)    
 
type DoublyLinkedList<'a when 'a : equality>() =
    let mutable head : DoublyNode<'a> option = None
    let mutable tail : DoublyNode<'a> option = None
    
    let addFront item =
        match head with
        | None ->
            let node = { Value = item; Next = None; Prev = None }
            head <- Some node
            tail <- Some node
        | Some headNode ->
            let node = { Value = item; Next = Some headNode; Prev = None }
            headNode.Prev <- Some node
            head <- Some node
            
            match tail with
            | Some tailNode ->
                if tailNode.Prev.IsNone then
                    tailNode.Prev <- Some node
            | _ -> failwith "list is in invalid state"
        
    
    let rec iterate head result =
        match head with
        | Some v -> iterate v.Next (v.Value::result)
        | None -> List.rev result          
    
    member _.Head with get() =
        match head with
        | Some x -> Some x.Value
        | None -> None
        
    member _.Tail with get() =
        match tail with
        | Some x -> Some x.Value
        | None -> None
        
    member _.AddFront item = addFront item
    member _.List() = iterate head List.empty