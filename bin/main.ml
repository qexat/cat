open Cat.Core
open Cat.Instance

let find_element (lst : 'a List.t) (value : 'a) =
  let rec aux list value start =
    match list with
    | Nil -> None
    | Cons (head, tail) -> if head = value then Some start else aux tail value (start + 1)
  in
  aux lst value 0
;;

let get_element (lst : 'a List.t) (index : int) : 'a Option.t =
  let rec aux lst index start =
    match lst with
    | Nil -> None
    | Cons (head, tail) -> if index = start then Some head else aux tail index (start + 1)
  in
  aux lst index 0
;;

let grades = List.of_std_list [ 16; 13; 15; 9; 12; 19 ]

let () =
  let open Option.Notation in
  match find_element grades 15 >>= get_element grades with
  | None -> Printf.printf "No grade of 15"
  | Some grade -> Printf.printf "Grade of %d\n" grade
;;

open Cat.Number
open Nat.Notation

let one = Nat.succ Nat.zero
let two = Nat.succ one
let three = one + two
let () = Printf.printf "three is %s\n" (Nat.render three)
