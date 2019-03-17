#light

module hw09

//
// mergesort L
//
// Sorts the given list using mergesort algorithm.
//
// Examples: mergesort [10; 9; 1; 0; 88; 2] => [0; 1; 2; 9; 10; 88]
// 
// NOTE: the code is given below, your task is to rewrite the
// merge function to be both safe (i.e. tail-recursive) and
// efficient.  You may use List.rev if need be, but no other
// List. functions may be used.
//

let splitAt n L =
  let rec splitUtil n L acc =
     match L with
     | []           -> List.rev acc, []
     | _ when n = 0 -> List.rev acc, L
     | x::tl        -> splitUtil (n-1) tl (x::acc)
  splitUtil n L []

let rec merge L1 L2 =
  //printfn "---ML1>%A<ML1---" L1
  //printfn "---ML2>%A<ML2---" L2

  let rec _merge L1 L2 L3 =
      match L1, L2 with
      | [], [] -> //printfn "---L3>%A<L3---" L3
                  L3
      | [], _  -> //printfn "---Right>%A<Right---" L2
                  L3 @ L2
      | _,  [] -> //printfn "---Left>%A<Left---" L1
                  L3 @ L1
      | hd1::tl1, hd2::tl2 when hd1 <= hd2 -> //printfn "---hd1>%A<hd1---" hd1
                                              _merge tl1 L2 (L3 @ [hd1])
      | hd1::tl1, hd2::tl2                 -> //printfn "---hd2>%A<hd2---" hd2
                                              _merge L1 tl2 (L3 @ [hd2])
  //let merged = 
  _merge L1 L2 []
  //printfn "---Merged>%A<Merged---" merged
  //merged


let rec mergesort L = 
  match L with
  | []         -> []
  | e::[]      -> [e]
  | e1::e2::[] -> if e1<=e2 then [e1;e2] else [e2;e1]
  | _          -> let mid = List.length L / 2
                  let (L1, L2) = splitAt mid L
                  //printfn "--L1>%A<L1--" L1
                  //printfn "--L2>%A<L2--" L2
                  merge (mergesort L1) (mergesort L2)
                  