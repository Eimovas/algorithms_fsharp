module StackMin

open StackMin
open Xunit

[<Fact>]
let ``Given empty stack, When peek, Then return None ``() =
    let stack = MinSimpleStack<int>()
    let result = stack.Peek()
    
    Assert.True result.IsNone

[<Fact>]
let ``Given non empty stack, When peek, Then return newest item``() =
    let stack = MinSimpleStack<int>()
    stack.Push 1
    stack.Push 2
    stack.Push 3
    
    let result = stack.Peek()
    
    Assert.Equal(3, result.Value)

[<Fact>]
let ``Given stack, When push, Then put newest item at the top of the stack``() =
    let stack = MinSimpleStack<int>()
    stack.Push 1
    stack.Push 2
    stack.Push 3
    
    let result = stack.Pop()
    
    Assert.Equal(3, result.Value)
    
[<Fact>]
let ``Given empty stack, When pop, Then return None``() =
    let stack = MinSimpleStack<int>()
    
    let result = stack.Pop()
    
    Assert.True result.IsNone
    
[<Fact>]
let ``Given non empty stack, When pop, Then return newest item``() =
    let stack = MinSimpleStack<int>()
    stack.Push 1
    stack.Push 2
    stack.Push 3
    
    let result = stack.Pop()    
    Assert.Equal(3, result.Value)
    
    stack.Push 4
    
    let result = stack.Pop()    
    Assert.Equal(4, result.Value)
    
[<Fact>]
let ``Given empty stack, When min, Then return None``() =
    let stack = MinSimpleStack<int>()
    
    let result = stack.Min()    
    Assert.True result.IsNone
    
[<Fact>]
let ``Given non empty stack, When min, Then return min item``() =
    let stack = MinSimpleStack<int>()
    stack.Push 2
    stack.Push 3
    
    let result = stack.Min()    
    Assert.Equal(2, result.Value)
    
    stack.Push 4
    stack.Push 1
    
    let result = stack.Min()    
    Assert.Equal(1, result.Value)
    
    stack.Pop() |> ignore
    stack.Pop() |> ignore
    
    let result = stack.Min()    
    Assert.Equal(2, result.Value)    