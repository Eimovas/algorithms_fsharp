module SumLists

open System
open System.Collections.Generic
open System.Text
open LinkedList

(*
You have two numbers represented by a linked list, where each node contains a single digit. The digits are stored
in a reverse order. Write function that adds the two number s and returns the sum as a reversed linked list.

Example:
Input: (7 - 1 - 6) + (5 - 9 - 2). That's 617+295
Output: 2 - 1 - 9. That's 912.

FOLLOW UP:
Do the same when linked lists are not reversed.

Questions:
- I don't understand that constraint of not being able to convert list to an integer.

Solution #1:
- Iterate first list, push digits on stack. Iterate stack and build number from it.
- Iterate second list, push digits on stack. Iterate stack and build number from it.
- Sum numbers, break result into digits, push into new list
- Time -> O(2x + 2y + z) -> O(n), where x,y,x - lengths of first, second, result lists
- Space -> O(stack size + result list) -> O(n)

Solution #2:
- Do a simple summation of digits, like in primary school:
    7 1 6
    5 9 2
    
    2 1 9
- Would this work with different length numbers?
    5 7 1 6
    5 9 2
    0 7 4 6
- Yeah this would probably work, but it might be a little complicated to implement.
- Time: O(max(x,y)) -> O(n), where x,y - lengths of lists
- Space: O(n) -> where n is length of result
- Looks same on paper as previous one, but this one should be much more efficient.
*)


let addListsNaive (first : SinglyLinkedList<char>) (second : SinglyLinkedList<char>) : SinglyLinkedList<char> =
    let rec pushToStack (stack : Stack<char>) (node : SinglyNode<char> option) =
        match node with
        | None -> stack
        | Some nodeValue ->
            stack.Push nodeValue.Value
            pushToStack stack nodeValue.Next
    let rec getIntFromStack (sb : StringBuilder) (stack : Stack<char>) =
        match stack.TryPop() with
        | true, value -> sb.Append value |> ignore; getIntFromStack  sb stack
        | _ -> sb |> string |> Int32.Parse
    
    if first.Head.IsNone && second.Head.IsNone then SinglyLinkedList<char>()
    elif first.Head.IsNone then second
    elif second.Head.IsNone then first
    else
        let firstDigit = pushToStack (Stack<char>()) first.Head |> getIntFromStack (StringBuilder())
        let secondDigit = pushToStack (Stack<char>()) second.Head |> getIntFromStack (StringBuilder())
        let resultChars = firstDigit + secondDigit |> string |> fun s -> s.ToCharArray()
        let result = SinglyLinkedList<char>()
        
        for c in resultChars do
            result.AddFront c
            
        result
