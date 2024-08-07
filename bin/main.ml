open Cat.Instance

let rec find_element_opt_acc lst value start =
  match lst with
  | [] -> Option.None
  | head :: tail ->
      if head = value then Option.Some start
      else find_element_opt_acc tail value (start + 1)

let find_element_opt lst value = find_element_opt_acc lst value 0

let rec get_element_opt_acc lst index start =
  match lst with
  | [] -> Option.None
  | head :: tail ->
      if index = start then Option.Some head
      else get_element_opt_acc tail index (start + 1)

let get_element_opt lst value = get_element_opt_acc lst value 0
let grades = [ 16; 13; 15; 9; 12; 19 ]

let () =
  let open Option in
  match find_element_opt grades 15 >>= get_element_opt grades with
  | None -> Printf.printf "No grade of 15"
  | Some grade -> Printf.printf "Grade of %d\n" grade

(* let l = LinkedList.lift 3
   let m = LinkedList.fmap (fun a -> a + 1) l
   let () = Printf.printf "%s\n" (LinkedList.pp m) *)

open Cat.Number

let one = Nat.s Nat.z
let two = Nat.s one

let three =
  let open Nat in
  one + two

let () = Printf.printf "three is %d\n" (Cat.Number.int_of_nat three)
