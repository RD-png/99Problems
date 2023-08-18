let rec last list =
  match list with
  | [] -> None
  | [x] -> Some x
  | x :: xs -> last xs
;;

let rec last_two list =
  match list with
  | [] -> None
  | x :: y :: [] -> Some (x, y)
  | hd :: xs -> last_two xs
;;

let rec nth index list =
  match index, list with
  | _, [] -> raise (Failure "nth")
  | 0, hd :: tl -> hd
  | index, hd :: tl -> nth (index - 1) tl
;;

let length list =
  let rec aux list acc =
    match list with
    | [] -> acc
    | hd :: tl -> aux tl (acc + 1)
  in
  aux list 0
;;

let reverse list =
  let rec aux list acc =
    match list with
    | [] -> acc
    | hd :: tl -> aux tl (hd :: acc)
  in
  aux list []
;;

let is_palindrome list =
  list = reverse list
;;

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten list =
  let rec aux list acc =
    match list with
    | [] -> acc
    | One x :: tl -> aux tl (x :: acc)
    | Many x :: tl -> aux x acc |> aux tl
  in
  aux list [] |> reverse
;;
