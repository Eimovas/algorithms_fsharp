module Queue

open Queue
open Xunit

[<Fact>]
let ``Given empty queue, When peek, Then return None ``() =
    let queue = SimpleQueue<int>()
    let result = queue.Peek()
    
    Assert.True result.IsNone

[<Fact>]
let ``Given non empty queue, When peek, Then return oldest item``() =
    let queue = SimpleQueue<int>()
    queue.Enqueue 1
    queue.Enqueue 2
    queue.Enqueue 3
    
    let result = queue.Peek()
    
    Assert.Equal(1, result.Value)

[<Fact>]
let ``Given queue, When enqueue, Then put newest item at the back of queue``() =
    let queue = SimpleQueue<int>()
    queue.Enqueue 1
    queue.Enqueue 2
    queue.Enqueue 3
    
    let result = queue.Dequeue()
    
    Assert.Equal(1, result.Value)
    
[<Fact>]
let ``Given empty queue, When dequeue, Then return None``() =
    let queue = SimpleQueue<int>()
    
    let result = queue.Dequeue()
    
    Assert.True result.IsNone
    
[<Fact>]
let ``Given non empty queue, When dequeue, Then oldest item``() =
    let queue = SimpleQueue<int>()
    queue.Enqueue 1
    queue.Enqueue 2
    queue.Enqueue 3
    
    let result = queue.Dequeue()    
    Assert.Equal(1, result.Value)
    
    queue.Enqueue 4
    
    let result = queue.Dequeue()    
    Assert.Equal(2, result.Value)
    