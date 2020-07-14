module StringRotation

open System.Text

(*
Assume you have a method isSubstring which checks if one word is a substring of another.
Given two strings, s1 and s2, write code to check if s2 is a rotation of s1 using only one call to isSubstring.
Example: waterbottle is a rotation of erbottlewat

Questions:
- Can I manipulate both strings before making call to isSubstring?
- What should happen if s1 is empty, and/or s2 is empty?

Notes:
- I could sort both strings and call method on sorted
    - Time - O(s1.length log s1.length + s2.length log s2.length)
    - Space - O(1)
    - This wouldn't always work though
- I could do s1+s1 (concat a copy of itself) and call the is substring on it
    - Time - O(s1.length) - copy operation?
    - Space - O(2 * s1.length)

*)

let private isSubstring (str1 : string) (str2 : string) = str2.Contains str1

let isRotation (str1 : string) (str2 : string) : bool =
    // TODO: add error handling.
    
    let updated = StringBuilder().Append(str1).Append(str1).ToString()
    isSubstring str2 updated
