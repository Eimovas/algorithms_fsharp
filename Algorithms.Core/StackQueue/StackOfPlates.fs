module StackOfPlates

open Stack

(*
Imagine a stack of plates. If the stack gets too hig, it might topple. Therefore, we would likely start a new stack when
the previous stack exceeds some threshold. Implement a data structure SetOfStacks that mimics this. SetOfStacks should
be composed of several stack and should create a new stack once the previous one exceeds capacity. Push and pop
should behave identically to a single stack.
 
FOLLOW UP:
implement a function popAt(int index) which performs pop operation on a specific sub-stack.


Notes:
- assuming the SetOfStacks takes an initial threshold argument.
- i could create an internal stack, which will hold other stacks
- i could also have an internal count holder which will indicate how many items are in current top stack, set it to 0 initially
- when i pop, i should:
    - get top stack (peek):
        - if its none, return none
        - if its some, pop item, reduce count, check count > 0:
            - if > 0, do nothing, return item
            - if = 0:
                - pop stack of stacks, peek stack of stacks:
                    - if its none, set count <- 0
                    - if its some, set count to threshold
- when i push, i should:
    - get the top stack:
        - if its none, create top stack, set count <- 1, push item to created stack, push stack to stacks
        - if its some, check count < threshold
            - if < threshold, increment count, push item to top stack
            - if = threshold, set count <- 1, create new stack, push item to it, push stack to stacks
*)

type SetOfStacks<'a>(threshold : int) =
    
    let stackOfStacks = SimpleStack<SimpleStack<'a>>()
    let mutable topStackItemCount = 0
    
    let pop() =
        match stackOfStacks.Peek() with
        | None -> None
        | Some stack ->
            let item = stack.Pop()
            topStackItemCount <- topStackItemCount - 1
            if topStackItemCount > 0 then item
            else
                stackOfStacks.Pop() |> ignore
                match stackOfStacks.Peek() with
                | None -> item
                | Some _ -> topStackItemCount <- threshold; item
    
    let peek() =
        match stackOfStacks.Peek() with
        | None -> None
        | Some topStack -> topStack.Peek()
        
    let push item =
        match stackOfStacks.Peek() with
        | None ->
            let stack = SimpleStack<'a>()
            stack.Push item
            topStackItemCount <- 1
            stackOfStacks.Push stack
        | Some stack when topStackItemCount < threshold ->
            topStackItemCount <- topStackItemCount + 1
            stack.Push item
        | Some _ when topStackItemCount = threshold ->
            let stack = SimpleStack<'a>()
            stack.Push item
            topStackItemCount <- 1
            stackOfStacks.Push stack
        | _ -> failwith "Stack is in an inconsistent state - this indicates a bug."
        
    member this.Pop() = pop()
    member this.Push item = push item
    member this.Peek() = peek()