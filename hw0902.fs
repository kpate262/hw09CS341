#light

module hw09

//
// delete e L
//
// Deletes all occurrences of e from the list L, 
// returning the new list.
//
// Examples: delete 3  [3; 1; 2]   => [1; 2]
//           delete 99 [99; 0; 99] => [0]
//           delete -2 []          => []
//           delete "cat" ["dog"]  => ["dog"]
//           delete 99999 [1..99999] => [1..99998]
// 
// NOTE: write a tail-recursive version using a helper
// function; do not change the API of the original 
// delete function.  You'll also need to build the new
// list efficiently; you can use List.rev if need be.
//

let rec concate e L list =
    match L with
    | [] -> List.rev list
    | hd::tail when hd = e -> concate e tail list
    | hd::tail -> concate e tail (hd :: list)
    
    
    
let delete e L = 
    let list = []
    concate e L list
