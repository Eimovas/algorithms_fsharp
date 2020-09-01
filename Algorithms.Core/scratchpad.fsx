#load "StackQueue/Stack.fs"
#load "StackQueue/StackMin.fs"

open StackMin

let stack = MinSimpleStack<int>()

stack.Push 9
stack.Min() |> printfn "%A"

stack.Push 1
stack.Min() |> printfn "%A"

stack.Push 3
stack.Min() |> printfn "%A"

stack.Pop()
stack.Min() |> printfn "%A"
stack.Pop()
stack.Min() |> printfn "%A"
stack.Pop()
stack.Min() |> printfn "%A"
