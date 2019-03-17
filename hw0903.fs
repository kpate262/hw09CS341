#light

module a

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
// NOTE: write this using a higher-order approach.
//
let delete e L = 
    List.filter (fun x -> x <> e) L

