module AnimalShelter

open AnimalShelter
open Xunit

[<Fact>]
let ``Given empty shelter, When dequeue any, Then return none``() =
    let shelter = AnimalShelter()
    let result = shelter.DequeueAny()
    Assert.True result.IsNone
    
[<Fact>]
let ``Given empty shelter, When dequeue dog, Then return none``() =
    let shelter = AnimalShelter()
    let result = shelter.DequeueDog()
    Assert.True result.IsNone
    
[<Fact>]
let ``Given empty shelter, When dequeue cat, Then return none``() =
    let shelter = AnimalShelter()
    let result = shelter.DequeueCat()
    Assert.True result.IsNone
    
[<Fact>]
let ``Given non empty shelter, When dequeue any, Then return oldest animal``() =
    let shelter = AnimalShelter()
    shelter.Enqueue Cat
    shelter.Enqueue Dog
    
    let result = shelter.DequeueAny()
    Assert.Equal(Cat, result.Value)
    
[<Fact>]
let ``Given non empty shelter, When dequeue cat, Then return cat``() =
    let shelter = AnimalShelter()
    shelter.Enqueue Cat
    
    let result = shelter.DequeueCat()
    Assert.Equal(Cat, result.Value)
    
[<Fact>]
let ``Given non empty shelter, When dequeue cat and no cats in shelter, Then return none``() =
    let shelter = AnimalShelter()
    shelter.Enqueue Dog
    
    let result = shelter.DequeueCat()
    Assert.True result.IsNone
    
[<Fact>]
let ``Given non empty shelter, When dequeue dog, Then return dog``() =
    let shelter = AnimalShelter()
    shelter.Enqueue Dog
    
    let result = shelter.DequeueDog()
    Assert.Equal(Dog, result.Value)
    
[<Fact>]
let ``Given non empty shelter, When dequeue dog and no dogs in shelter, Then return none``() =
    let shelter = AnimalShelter()
    shelter.Enqueue Cat
    
    let result = shelter.DequeueDog()
    Assert.True result.IsNone
    
