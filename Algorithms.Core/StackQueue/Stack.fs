module Stack

type private SimpleStackNode<'a> = {
    mutable Next : SimpleStackNode<'a> option
    Value : 'a
}

type SimpleStack<'a>() =
    let mutable top : SimpleStackNode<'a> option = None
    
    let pop() = 
        match top with
        | None -> None
        | Some x ->
            top <- x.Next
            Some x.Value
            
    let push (item : 'a) =
        match top with
        | None -> top <- Some { Next = None; Value = item }
        | Some _ -> 
            let node = { Next = top; Value = item }
            top <- Some node
            
    let peek() =
        match top with
        | None -> None
        | Some x -> Some x.Value
            
    member this.Pop() = pop()
    member this.Push item = push item
    member this.Peek() = peek()
    member this.IsEmpty() = top.IsNone

