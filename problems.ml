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
    | One x :: tl -> x :: acc |> aux tl
    | Many x :: tl -> aux x acc |> aux tl
  in
  aux list [] |> reverse
;;

let compress lst =
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | hd :: (hd' :: _ as tl) when hd = hd' -> aux tl acc
    | hd :: tl -> hd :: acc |> aux tl
  in
  aux lst [] |> reverse
;;

let pack lst =
  let rec aux lst acc =
    match lst with
    | [] -> []
    | hd :: (hd' :: _ as tl) when hd = hd' -> hd :: acc |> aux tl
    | hd :: tl -> (hd :: acc) :: (aux tl [])
  in
  aux lst []
;;

let encode lst =
  let packed = pack lst in
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | [] :: _ -> acc
    | (hd :: _) as pck :: tl -> (length pck, hd) :: acc |> aux tl
  in
  aux packed [] |> reverse
;;
