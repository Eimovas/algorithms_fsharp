module SortStack

open Stack

(*
Write a stack, which always keeps the smallest item on the top of the stack. You can use
an additional temporary stack, but you may not copy elements into any other data structure, such as an array.
Stack supports push, pop, peek, isEmpty.

Notes:
- When adding a new element, i need to check whether new element is smaller (or top element is None) than top element on stack:
    - if yes, just push it to top, push items from temp stack back to main stack
    - if no:
        - pop top element to temp stack, repeat comparison of top element vs item

*)

type SortStack<'a when 'a : comparison>() =
    let tempStack = SimpleStack<'a>()
    let mainStack = SimpleStack<'a>()
    
    let rec pushTempToMain() =
        match tempStack.Pop() with
        | None -> ()
        | Some x -> mainStack.Push x; pushTempToMain()
    
    let rec push item =
        match mainStack.Peek() with
        | None ->
            mainStack.Push item
            pushTempToMain()
        | Some x when item <= x ->
            mainStack.Push item
            pushTempToMain()
        | Some x ->
            mainStack.Pop() |> ignore
            tempStack.Push x
            push item
    
    member this.Push item = push item
    member this.Pop() = mainStack.Pop()
    member this.Peek() = mainStack.Peek()