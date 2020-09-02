#load "StackQueue/Stack.fs"
#load "StackQueue/SortStack.fs"

open SortStack

let stack = SortStack<int>()

stack.Peek() |> printfn "%A"

stack.Push 1
stack.Push 2
stack.Push 3
stack.Pop() |> printfn "%A"
stack.Pop() |> printfn "%A"
stack.Pop() |> printfn "%A"
