module StackMin

(*
Design a stack, which, in addition to push and pop, has a method min(), which returns a current minimum value
in the stack. Time constraints: all operations must be a O(1).

Ideas:
- I could have a simple private stack which contains the current min element at the top of the stack, which i can peek.
- When item is added to stack, i need to check if min value changes, and if it does, push new value to stack
- When item is popped, i need to check if returned item is equal the current min value, if it is - pop min value

*)

open Stack

type MinSimpleStack<'a when 'a : comparison>() =
    let minStack = SimpleStack<'a>()
    let valueStack = SimpleStack<'a>()
    
    member this.Min() = minStack.Peek()
    member this.Pop() =
        match minStack.Peek() with
        | None -> None
        | Some min ->
            let current = valueStack.Pop().Value // if min is not none, this will always exist
            if min = current then
                minStack.Pop() |> ignore
                Some current
            else
                Some current
    member this.Push item =
        match minStack.Peek() with
        | None ->
            minStack.Push item
            valueStack.Push item
        | Some min ->
            if item <= min then
                minStack.Push item
                valueStack.Push item
            else valueStack.Push item
    member this.Peek() = valueStack.Peek()

