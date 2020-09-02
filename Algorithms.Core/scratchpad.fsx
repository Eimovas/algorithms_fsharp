#load "StackQueue/Stack.fs"
#load "StackQueue/QueueFromStacks.fs"

open QueueFromStacks

let stack = StackQueue<int>()

stack.Enqueue 9
stack.Enqueue 1
stack.Enqueue 3
stack.Dequeue() |> printfn "%A"
stack.Dequeue() |> printfn "%A"
stack.Dequeue() |> printfn "%A"
