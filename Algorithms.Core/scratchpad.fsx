#load "StackQueue/Queue.fs"

open Queue

let rec iterateQueue (queue : SimpleQueue<'a>) =
    match queue.Dequeue() with
    | None -> printfn "Queue finished"
    | Some x -> printfn "Value: %A" x; iterateQueue queue
    

let queue = SimpleQueue<int>()

queue.Enqueue 1
queue.Enqueue 2
queue.Enqueue 3
queue.Enqueue 4

iterateQueue queue

