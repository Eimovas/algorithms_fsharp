module SortStack

open SortStack
open Xunit

[<Fact>]
let ``Given empty stack, When peek, Then return None ``() =
    let stack = SortStack<int>()
    let result = stack.Peek()
    
    Assert.True result.IsNone

[<Fact>]
let ``Given non empty stack, When peek, Then return smallest item``() =
    let stack = SortStack<int>()
    stack.Push 1
    stack.Push 2
    stack.Push 3
    
    let result = stack.Peek()
    
    Assert.Equal(1, result.Value)

[<Fact>]
let ``Given stack, When push, Then put smallest item at the top of the stack``() =
    let stack = SortStack<int>()
    stack.Push 1
    stack.Push 2
    stack.Push 3
    
    let result = stack.Pop()
    
    Assert.Equal(1, result.Value)
    
[<Fact>]
let ``Given empty stack, When pop, Then return None``() =
    let stack = SortStack<int>()
    
    let result = stack.Pop()
    
    Assert.True result.IsNone
    
[<Fact>]
let ``Given non empty stack, When pop, Then return smallest item``() =
    let stack = SortStack<int>()
    stack.Push 1
    stack.Push 2
    stack.Push 3
    
    let result = stack.Pop()    
    Assert.Equal(1, result.Value)
    
    stack.Push 0
    
    let result = stack.Pop()    
    Assert.Equal(0, result.Value)
    