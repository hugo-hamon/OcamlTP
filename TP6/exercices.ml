let ( -- ) x y =
  if x <= y then List.init (y - x + 1) (( + ) x)
  else List.init (x - y + 1) (( - ) x)

(*Exercice 1*)

let is_prefix xs ys =
  let rec is_prefix_aux xs ys res =
    match xs with
    | [] -> res
    | _ when ys = [] -> false
    | xs when List.hd xs = List.hd ys ->
        is_prefix_aux (List.tl xs) (List.tl ys) res
    | _ -> is_prefix_aux [] ys false
  in
  is_prefix_aux xs ys true

let is_suffix xs ys = is_prefix (List.rev xs) (List.rev ys)

let strip_prefixe xs ys =
  if is_prefix xs ys = false then failwith "xs must be a prefix of ys"
  else
    let rec strip_prefixe_aux xs ys =
      match xs with
      | [] -> ys
      | xs when List.hd xs = List.hd ys ->
          strip_prefixe_aux (List.tl xs) (List.tl ys)
      | _ -> ys
    in
    strip_prefixe_aux xs ys

let strip_prefixe_opt xs ys =
  if is_prefix xs ys = false then None else Some (strip_prefixe xs ys)

(*Exercice 2*)

let even x = x mod 2 = 0

let find_index_even f xs =
  let rec find_index_even_aux f xs index =
    match xs with
    | [] -> failwith "element ot found"
    | h :: t when f h = true -> index
    | _ -> find_index_even_aux f (List.tl xs) (index + 1)
  in
  find_index_even_aux f xs 0

let find_indice_opt f xs =
  match xs with
  | xs
    when let aux = true in
         List.fold_left
           (fun x y -> if f x = false then aux && false else aux && true)
           aux xs
         = false ->
      None
  | _ -> Some (find_index_even f xs)

let find_indices f xs =
  let rec find_indices_aux f xs res indice =
    match xs with
    | [] -> res
    | h :: t when f h = true -> find_indices_aux f t (indice :: res) (indice + 1)
    | _ -> find_indices_aux f (List.tl xs) res (indice + 1)
  in
  find_indices_aux f xs [] 0

exception Not_Found

let elem_index element xs =
  let rec elem_index_aux element xs indice =
    match xs with
    | [] -> raise Not_Found
    | h :: t when h = element -> indice
    | _ -> elem_index_aux element (List.tl xs) (indice + 1)
  in
  elem_index_aux element xs 0

let elem_index_opt element xs =
  try Some (elem_index element xs) with Not_Found -> None

let elem_indices element xs = find_indices (fun x -> x = element) xs

(*Exercice 3*)

let drop n xs =
  let rec drop_aux n xs res =
    match xs with
    | [] -> res
    | h :: t when n = 0 -> drop_aux n t (res @ [ h ])
    | _ -> drop_aux (n - 1) (List.tl xs) res
  in
  drop_aux n xs []

let tails xs =
  let rec tails_aux xs res =
    match xs with
    | [] -> res @ [ [] ]
    | _ -> tails_aux (List.tl xs) (res @ [ xs ])
  in
  tails_aux xs []

let init xs = List.rev (List.map List.rev (tails (List.rev xs)))

let is_in xs ys =
  let rec is_in_aux xs ys res =
    match ys with
    | [] -> res
    | h :: _ when xs = h -> true
    | _ -> is_in_aux xs (List.tl ys) res
  in
  is_in_aux xs ys false

let is_infix xs ys =
  let rec is_infix_aux xs ys res =
    match ys with
    | [] -> res
    | ys when is_in xs (init ys) -> true
    | _ -> is_infix_aux xs (List.tl ys) res
  in
  is_infix_aux xs ys false

(* Exercice 4*)

let equiv_mod_3 x y = x mod 3 = y mod 3

let delete_by cp x ys =
  let rec delete_by_aux cp x ys res =
    match ys with
    | [] -> res
    | h :: t when cp x h -> res @ t
    | _ -> delete_by_aux cp x (List.tl ys) (res @ [ List.hd ys ])
  in
  delete_by_aux cp x ys []

let delete x xs = delete_by (fun x y -> x = y) x xs

let diff_by cp xs ys =
  List.fold_left (fun x y -> delete_by cp x ys @ [ y ]) [] xs

let _ = diff_by equiv_mod_3 (1 -- 10) ((5 -- 15) @ (1 -- 10))
