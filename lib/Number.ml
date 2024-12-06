module NatMinimalFunctor = struct
  type 'a t =
    | O
    | S of 'a

  let functor_map (func : 'a -> 'b) : 'a t -> 'b t = function
    | O -> O
    | S value -> S (func value)
  ;;
end

module Nat = struct
  (* Let's first build our full functor *)
  include NatMinimalFunctor
  module Functor = Factory.Functor (NatMinimalFunctor)

  (* Let's introduce the fixpoint *)
  include Iter.Fix (Functor)

  type t = fix

  (* Then our catamorphism *)
  module Catamorphism = Morphism.Catamorphism (Functor)

  let zero : t = Fix O
  let succ (n : t) : t = Fix (S n)

  let pred : t -> t = function
    | Fix (S n) -> n
    | zero -> zero
  ;;

  let add (n : t) : t -> t =
    Catamorphism.cata (function
      | O -> n
      | S m' -> succ m')
  ;;

  let sub (n : t) : t -> t =
    Catamorphism.cata (function
      | O -> n
      | S m' -> pred m')
  ;;

  let rec to_int : t -> int = function
    | Fix O -> 0
    | Fix (S n) -> Int.add 1 (to_int n)
  ;;

  let of_int (i : int) : t Instance.Option.t =
    let open Instance in
    if i < 0
    then Option.None
    else (
      let integer = ref i in
      let result = ref (Fix O) in
      while i > 0 do
        result := Fix (S !result);
        integer := !integer - 1
      done;
      Option.Some !result)
  ;;

  let of_int_exn (i : int) : t =
    match of_int i with
    | None -> failwith "integer must be non-negative"
    | Some value -> value
  ;;

  let render (n : t) : string = n |> to_int |> Int.to_string

  module Notation = struct
    let ( + ) : t -> t -> t = add
    let ( - ) : t -> t -> t = sub
  end
end
