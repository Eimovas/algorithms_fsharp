module AnimalShelter

open System
open Queue

(*
An animal shelter, which holds only dogs and cats, operates on a strictly first in, first out basis. People must
either adopt the oldest (based on arrival time) of all animals at the shelter or they can select whether they want
a cat or a dog (and will receive the oldest animal of that type). They can't select which specific animal they want.
Create a the data structures to maintain this system and implement operations such as enqueue, dequeueAny, dequeueDog,
dequeueCat.

Solution #1:
- I could create three queues - one for dogs, one for cats, one for all.
- When enqueueing, i would add animal to either dogs or cats, and to the one for all
- When dequeueAny, I would dequeue all queue, inspect item, and would dequeue either dog or cat queue too
- When dequeueCat/Dog, I would dequeue it and would dequeue any (animals should match)
- Time - O(1), Space - O(N) - the ANY queue will take up extra space
- I could create an internal node to wrap animal and store only reference in the ANY queue - it would reduce space.

Solution #2:
- I could create a queue using linked list and for dequeueCat/Dog, I would iterate list from head looking for that animal.
- When found, I would update the list and return it.
- DequeueAny is a simple return of head.
- Time (cat/dog) - O(n), Time (any) - O(1), Space - O(1)

Solution #3 (optimal):
- I could create two queues, for cats and dogs.
- I will create a wrapper node to contain the timestamp of when animal was added
- When dequeueAny, I inspect both queue heads and choose newest.
- Time - O(1), Space - O(1) (although I add a timestamp, which might be > of reference in solution #1)
*)

type Animal =
    | Cat 
    | Dog
    
type private AnimalTimestamped = { Animal : Animal; Timestamp : DateTime }

type AnimalShelter() =
    let cats = SimpleQueue<AnimalTimestamped>()
    let dogs = SimpleQueue<AnimalTimestamped>()
    
    let dequeueAny() =
        match dogs.Peek(), cats.Peek() with
        | None, None -> None
        | Some _, None -> dogs.Dequeue() |> ignore; Some Dog
        | None, Some _ -> cats.Dequeue() |> ignore; Some Cat
        | Some dog, Some cat when dog.Timestamp < cat.Timestamp -> dogs.Dequeue() |> ignore; Some Dog
        | Some dog, Some cat when dog.Timestamp >= cat.Timestamp -> cats.Dequeue() |> ignore; Some Cat
        | _ -> failwith "Unexpected state of data structure which indicates a bug."
    
    let enqueue = function
        | Cat -> cats.Enqueue { Animal = Cat; Timestamp = DateTime.UtcNow }
        | Dog -> dogs.Enqueue { Animal = Dog; Timestamp = DateTime.UtcNow }
        
    let dequeueDog() =
        match dogs.Dequeue() with
        | None -> None
        | Some _ -> Some Dog
        
    let dequeueCat() =
        match cats.Dequeue() with
        | None -> None
        | Some _ -> Some Cat
    
    member this.DequeueAny = dequeueAny
    member this.DequeueDog = dequeueDog
    member this.DequeueCat = dequeueCat
    member this.Enqueue = enqueue