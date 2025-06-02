(** Various instances of the typeclasses. *)

open Core

(* Examples of functor instances *)

module Predicate = struct
  module Base = struct
    type 'a t = 'a -> bool

    let map (func : 'a -> 'b) (predicate : 'b t) : 'a t = fun a -> predicate (func a)
  end

  module Full = Mixins.Contravariant (Base)
  module Notation = Notation.Contravariant (Full)
  include Full
  include Base
end

module Box = struct
  module Base = struct
    type 'a t = Box of 'a

    let map (func : 'a -> 'b) : 'a t -> 'b t = function
      | Box a -> Box (func a)
    ;;

    let lift (value : 'a) : 'a t = Box value

    let lift_binary (func : 'a -> 'b -> 'c) (box1 : 'a t) (box2 : 'b t) : 'c t =
      match map func box1 with
      | Box f -> map f box2
    ;;

    let select (box : ('a, 'b) Either.t t) (func : ('a -> 'b) t) : 'b t =
      match box with
      | Box (Left a) -> map (( |> ) a) func
      | Box (Right b) -> Box b
    ;;

    let bind (box : 'a t) (func : 'a -> 'b t) : 'b t =
      match map func box with
      | Box value -> value
    ;;
  end

  module Full = Mixins.Monad (Base)
  module Notation = Notation.Monad (Full)
  include Full
  include Base
end

module Homo_pair = struct
  module Base = struct
    type 'a t = 'a * 'a

    let map (func : 'a -> 'b) : 'a t -> 'b t = function
      | left, right -> func left, func right
    ;;

    let lift (value : 'a) : 'a t = value, value

    let lift_binary (func : 'a -> 'b -> 'c) (pair1 : 'a t) (pair2 : 'b t) : 'c t =
      match pair1, pair2 with
      | (left1, left2), (right1, right2) -> func left1 right1, func left2 right2
    ;;
  end

  (* We generate the rest of the bindings using a factory *)
  module Full = Mixins.Applicative (Base)

  (* It's nice to have notation so let's add that too *)
  module Notation = Notation.Applicative (Full)

  (* We just need to populate the present module *)
  include Full
  include Base
end

module Option = struct
  (* Base definition *)
  module Base = struct
    type 'a t = 'a option

    let map (func : 'a -> 'b) : 'a t -> 'b t = function
      | None -> None
      | Some value -> Some (func value)
    ;;

    let lift (value : 'a) : 'a t = Some value

    let lift_binary (func : 'a -> 'b -> 'c) (opt1 : 'a t) (opt2 : 'b t) =
      match map func opt1 with
      | Some rest -> map rest opt2
      | None -> None
    ;;

    let select (opt : ('a, 'b) Either.t t) (func : ('a -> 'b) t) : 'b t =
      match opt with
      | None -> None
      | Some (Left a) -> map (( |> ) a) func
      | Some (Right b) -> Some b
    ;;

    let bind (opt : 'a t) (func : 'a -> 'b t) : 'b t =
      match map func opt with
      | None -> None
      | Some opt -> opt
    ;;
  end

  (* We generate the rest of the bindings using a factory *)
  module Full = Mixins.Monad (Base)

  (* We just need to populate the present module *)
  include Full
  include Base

  (* It's nice to have notation so let's add that too *)
  module Notation = struct
    include Notation.Monad (Full)

    let ( or ) (option : 'a t) (default : 'a) : 'a =
      match option with
      | None -> default
      | Some value -> value
    ;;
  end
end

module List = struct
  (* Base definition *)
  module Base = struct
    type 'a t = 'a Core.list

    let rec map (func : 'a -> 'b) : 'a t -> 'b t = function
      | Nil -> Nil
      | Cons (value, next) -> Cons (func value, map func next)
    ;;

    let lift (value : 'a) : 'a t = Cons (value, Nil)

    let lift_binary (func : 'a -> 'b -> 'c) (list1 : 'a t) (list2 : 'b t) : 'c t =
      let rec aux func list1 list2 acc =
        match list1, list2 with
        | Nil, Nil -> acc
        | Cons (value1, next1), Cons (value2, next2) ->
          aux func next1 next2 (Cons (func value1 value2, acc))
        | _ -> failwith "mismatched list lengths"
      in
      aux func list1 list2 Nil
    ;;

    let reverse (list : 'a t) : 'a t =
      let rec aux list acc =
        match list with
        | Nil -> acc
        | Cons (head, tail) -> aux tail (Cons (head, acc))
      in
      aux list Nil
    ;;

    let concatenate (list1 : 'a t) (list2 : 'a t) : 'a t =
      let rec aux list1 list2 acc =
        match list1 with
        | Nil -> reverse acc
        | Cons (head, tail) -> aux tail list2 (Cons (head, acc))
      in
      aux list1 list2 Nil
    ;;

    let rec fold (func : 'acc -> 'a -> 'acc) (initial : 'acc) : 'a t -> 'acc = function
      | Nil -> initial
      | Cons (first, rest) -> fold func (func initial first) rest
    ;;

    let rec select (list : ('a, 'b) Either.t t) (func : ('a -> 'b) t) : 'b t =
      match list with
      | Nil -> Nil
      | Cons (Left a, rest) -> concatenate (map (( |> ) a) func) (select rest func)
      | Cons (Right b, rest) -> Cons (b, select rest func)
    ;;

    let bind (list : 'a t) (func : 'a -> 'b t) : 'b t =
      match map func list with
      | Nil -> Nil
      | Cons (first, rest) -> fold concatenate first rest
    ;;
  end

  (* We generate the rest of the bindings using a factory *)
  module Full = Mixins.Monad (Base)

  (* We just need to populate the present module *)
  include Full
  include Base

  (* It's nice to have notation so let's add that too *)
  module Notation = Notation.Monad (Full)

  let map_std_list (func : 'a -> 'b) (list : 'a Stdlib.List.t) : 'b t =
    let rec aux (func : 'a -> 'b) (list : 'a Stdlib.List.t) (acc : 'b t) : 'b t =
      match list with
      | [] -> reverse acc
      | head :: tail -> aux func tail (Cons (func head, acc))
    in
    aux func list Nil
  ;;

  let of_std_list (list : 'a Stdlib.List.t) : 'a t = map_std_list (fun a -> a) list
end

module List_by_rec = struct
  module Base = struct
    include Iter.Fix_tag (Option)

    type 'a t = 'a fix

    let rec map (func : 'a -> 'b) : 'a t -> 'b t = function
      | Empty -> Empty
      | Tagged (first, rest) -> Tagged (func first, Option.map (map func) rest)
    ;;

    let lift (value : 'a) : 'a t = Tagged (value, None)
  end

  module Full = Mixins.Functor (Base)
  include Full
  include Base
  module Notation = Notation.Functor (Full)
end

(* Examples of bifunctor instances *)

module Either = struct
  module Base : Typeclass.BIFUNCTOR = struct
    type ('a, 'b) t =
      | Left of 'a
      | Right of 'b

    let map (funcl : 'a -> 'c) (funcr : 'b -> 'd) : ('a, 'b) t -> ('c, 'd) t = function
      | Left left -> Left (funcl left)
      | Right right -> Right (funcr right)
    ;;
  end

  module Full = Mixins.Bifunctor (Base)
  include Full
  include Base
end
