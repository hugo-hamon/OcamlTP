(* Exercice 1*)

type 'a tree = Empty | Node of ('a * 'a tree * 'a tree)

let rec is_in e = function
  | Empty -> false
  | Node (a, _, _) when a = e -> true
  | Node (_, at, bt) -> is_in e at || is_in e bt

let rec hauteur = function
  | Empty -> 0
  | Node (_, at, bt) -> 1 + max (hauteur at) (hauteur bt)

let rec nbr_element = function
  | Empty -> 0
  | Node (_, at, bt) -> 1 + nbr_element at + nbr_element bt

let left_value bt =
  let rec aux acc = function
    | Empty -> acc
    | Node (a, at, _) -> aux (Some a) at
  in
  aux None bt

let right_value bt =
  let rec aux acc = function
    | Empty -> acc
    | Node (a, _, bt) -> aux (Some a) bt
  in
  aux None bt

let rec infixe = function
  | Empty -> []
  | Node (a, at, bt) -> infixe at @ (a :: infixe bt)

let rec prefixe = function
  | Empty -> []
  | Node (a, at, bt) -> (a :: infixe at) @ infixe bt

let rec sufixe = function
  | Empty -> []
  | Node (a, at, bt) -> infixe at @ infixe bt @ [ a ]

let max_bt bt =
  let rec aux acc = function
    | Empty -> acc
    | Node (a, at, bt) -> max (aux (Some a) at) (aux (Some a) bt)
  in
  aux None bt

let min_bt bt =
  let rec aux acc = function
    | Empty -> acc
    | Node (a, at, bt) -> min (aux (Some a) at) (aux (Some a) bt)
  in
  aux None bt

(* Exercice 2 *)
let rec is_in2 e = function
  | Empty -> false
  | Node (a, _, _) when a = e -> true
  | Node (a, at, bt) -> if e < a then is_in2 e at else is_in2 e bt

let min_value bt = left_value bt

let max_value bt = right_value bt

let rec is_binary_tree = function
  | Empty -> true
  | Node (a, at, bt) ->
      if max_bt at > Some a || min_bt bt < Some a then false
      else is_binary_tree at && is_binary_tree bt

let rec add_value_bt value = function
  | Empty -> Node (value, Empty, Empty)
  | Node (a, at, bt) ->
      if a > value then Node (a, add_value_bt value at, bt)
      else Node (a, at, add_value_bt value bt)

let array_to_bt arr =
  let rec aux acc = function
    | [] -> acc
    | t :: q -> aux (add_value_bt t acc) q
  in
  aux Empty arr

let rec delete_value_bt value = function
  | Empty -> Empty
  | Node (a, at, bt) when a = value ->
      if bt = Empty then at
      else if at = Empty then bt
      else delete_value_bt (Option.get (min_value bt)) bt
  | Node (a, at, bt) ->
      if value > a then delete_value_bt value bt else delete_value_bt value at

(* trie *)
let rec mult_arr value = function
  | [] -> 0
  | t :: q when t = value -> 1 + mult_arr value q
  | t :: q -> mult_arr value q

let rec is_in value = function
  | [] -> false
  | t :: q when t = value -> true
  | t :: q -> is_in value q

let unique_arr arr =
  let rec aux acc = function
    | [] -> acc
    | t :: q when is_in t q -> aux acc q
    | t :: q -> aux (acc @ [ t ]) q
  in
  aux [] arr

let arr_cp arr =
  List.fold_left (fun x y -> x @ [ (y, mult_arr y arr) ]) [] (unique_arr arr)

let _ = infixe (array_to_bt (arr_cp [ 5; 9; 2; 4; 3; 1; 2; 2 ]))

let dec (el, nb) =
  let rec aux acc = function
    | _, b when b = 0 -> acc
    | a, b -> aux (a :: acc) (a, b - 1)
  in
  aux [] (el, nb)

let trie arr =
  List.fold_left (fun x y -> x @ dec y) [] (infixe (array_to_bt (arr_cp arr)))

let trie2 arr = List.concat (List.map dec (infixe (array_to_bt (arr_cp arr))))

let _ = trie2 [ 5; 9; 2; 4; 5; 1; 10 ]
