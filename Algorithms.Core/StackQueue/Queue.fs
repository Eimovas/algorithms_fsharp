module Queue

type private SimpleQueueNode<'a> = {
    mutable Next : SimpleQueueNode<'a> option
    Value : 'a
}

type SimpleQueue<'a>() =
    let mutable front : SimpleQueueNode<'a> option = None
    let mutable back : SimpleQueueNode<'a> option = None
    
    let peek() =
        match front with
        | None -> None
        | Some x -> Some x.Value
    
    let enqueue (item : 'a) =
        match front,back with
        | None, None ->
            let node = { Next = None; Value = item }
            front <- Some node
            back <- Some node
        | Some f, Some b when f.Next.IsNone ->
            let node = { Next = None; Value = item }
            b.Next <- Some node
            back <- Some node
            f.Next <- back
        | Some _, Some b ->
            let node = { Next = None; Value = item }
            b.Next <- Some node
            back <- Some node
        | _ -> failwith "The current state of queue is unexpected and invalid. This should never happen and indicates there's a bug in Queue's implementation"
    
    let dequeue() =
        match front,back with
        | None, None -> None
        | Some f, Some _ ->
            if f.Next.IsNone then
                front <- None
                back <- None
                Some f.Value 
            else
                front <- f.Next
                Some f.Value
        | _ -> failwith "The current state of queue is unexpected and invalid. This should never happen and indicates there's a bug in Queue's implementation"
    
    member this.Peek() = peek()
    member this.Enqueue item = enqueue item
    member this.Dequeue() = dequeue()
    member this.IsEmpty() = back.IsNone
