#light

module hw09

//
// subset S L
//
// Returns true if S is a subset of L, false if not.
// 
// Examples: subset [3; 1; 2] [1; 4; 2; 0; 3] => yes
//           subset [2; 1; 3] [1; 4; 0; 2; 8] => no
//           subset [] []                     => yes
//           subset [] [1; 2; 3]              => yes
//           subset [1..1000] [1..1000]       => yes
// 
// NOTE: you can solve using tail-recursion, or using
// higher-order approach.  Whatever you prefer.  Beware
// that List.length is an O(N) operation, it actually
// traverses the list and counts the elements.
// 
// HINT: there are a variety of solutions.  One idea
// is write a helper function "contains e L" that 
// returns true if e is an element of L, and false 
// if not.  Then build on that to define subset.  This
// leads to an O(N^2) solution, which is fine.
// 


let rec contains L i =
    match L with
    | [] -> false
    | e::tail when e = i -> true
    | e::tail -> contains tail i

let rec subset S L = 
    match S with
    | [] -> true
    | e::tail when (contains L e) = false -> false
    | e::tail when (contains L e) = true -> subset tail L
    
    
