#load "StackQueue/Stack.fs"

open Stack

let rec iterateStack (stack : SimpleStack<'a>) =
    match stack.Pop() with
    | None -> printfn "Stack finished"
    | Some x -> printfn "Value: %A" x; iterateStack stack
    

let stack = SimpleStack<int>()

stack.Push 1
stack.Push 2
stack.Push 3
stack.Push 4

iterateStack stack

