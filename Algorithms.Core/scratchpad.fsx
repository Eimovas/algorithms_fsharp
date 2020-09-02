#load "StackQueue/Stack.fs"
#load "StackQueue/StackOfPlates.fs"

open StackOfPlates

let stack = SetOfStacks<int>(2)

stack.Push 9
stack.Push 1
stack.Push 3
stack.Pop() |> printfn "%A"
stack.Pop() |> printfn "%A"
stack.Pop() |> printfn "%A"
