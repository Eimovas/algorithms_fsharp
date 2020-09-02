module QueueFromStacks

open Stack

(*
Implement a queue using two stacks.

Notes:
- Every time a dequeue is called, move all items from #1 to #2, so oldest is at the top, and pop item from it
- Every time a peek is called, move all items from #1 to #2, so oldest is at the top, and peek it
- Every time an enqueue is called, ensure all items are moved from #2 to #1, so newest is at the top, and push it top the top
*)

type StackQueue<'a>() =
    let stack1 = SimpleStack<'a>()
    let stack2 = SimpleStack<'a>()
    
    let rec moveFrom1To2() =
        match stack1.Pop() with
        | None -> ()
        | Some x -> stack2.Push x; moveFrom1To2() 
        
    let rec moveFrom2To1() =
        match stack2.Pop() with
        | None -> ()
        | Some x -> stack1.Push x; moveFrom2To1() 
        
    let dequeue() =
        moveFrom1To2() |> ignore
        stack2.Pop()
    let peek() =
        moveFrom1To2() |> ignore
        stack2.Peek()
    let enqueue item =
        moveFrom2To1() |> ignore
        stack1.Push item
    
    member this.Enqueue item = enqueue item
    member this.Dequeue() = dequeue()
    member this.Peek() = peek()

