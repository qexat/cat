(** Various instances of the functorial typeclasses. *)

(* Examples of contravariant functor instances *)

module Predicate = struct
  module Base = struct
    type 'a t = 'a -> bool

    let functor_map (func : 'a -> 'b) (predicate : 'b t) : 'a t =
      fun a -> predicate (func a)
    ;;
  end

  module Full = Factory.Contravariant_functor (Base)
  module Notation = Notation_factory.Contravariant_functor (Full)
  include Full
  include Base
end

(* Examples of bifunctor instances *)

module Either = struct
  module Base : Typeclass.BIFUNCTOR_BASE = struct
    type ('a, 'b) t =
      | Left of 'a
      | Right of 'b

    let functor_map (funcl : 'a -> 'c) (funcr : 'b -> 'd) : ('a, 'b) t -> ('c, 'd) t
      = function
      | Left left -> Left (funcl left)
      | Right right -> Right (funcr right)
    ;;
  end

  module Full = Factory.Bifunctor (Base)
  include Full
  include Base
end

(* Examples of applicative instances *)

module HomoPair = struct
  module Base = struct
    type 'a t = 'a * 'a

    let functor_map (func : 'a -> 'b) : 'a t -> 'b t = function
      | left, right -> func left, func right
    ;;

    let lift (value : 'a) : 'a t = value, value

    let lift_binary (func : 'a -> 'b -> 'c) (pair1 : 'a t) (pair2 : 'b t) : 'c t =
      match pair1, pair2 with
      | (left1, left2), (right1, right2) -> func left1 right1, func left2 right2
    ;;
  end

  (* We generate the rest of the bindings using a factory *)
  module Full = Factory.Applicative (Base)

  (* It's nice to have notation so let's add that too *)
  module Notation = Notation_factory.Applicative (Full)

  (* We just need to populate the present module *)
  include Full
  include Base
end

(* Examples of monad instances *)

module Option = struct
  (* Base definition *)
  module Base = struct
    type 'a t =
      | None
      | Some of 'a

    let functor_map (func : 'a -> 'b) : 'a t -> 'b t = function
      | None -> None
      | Some value -> Some (func value)
    ;;

    let lift (value : 'a) : 'a t = Some value

    let lift_binary (func : 'a -> 'b -> 'c) (opt1 : 'a t) (opt2 : 'b t) =
      match functor_map func opt1 with
      | Some rest -> functor_map rest opt2
      | None -> None
    ;;

    let join : 'a t t -> 'a t = function
      | None -> None
      | Some value -> value
    ;;
  end

  (* We generate the rest of the bindings using a factory *)
  module Full = Factory.Monad (Base)

  (* We just need to populate the present module *)
  include Full
  include Base

  (* It's nice to have notation so let's add that too *)
  module Notation = struct
    include Notation_factory.Monad (Full)

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
    type 'a t =
      | Nil
      | Cons of ('a * 'a t)

    let rec functor_map (func : 'a -> 'b) : 'a t -> 'b t = function
      | Nil -> Nil
      | Cons (value, next) -> Cons (func value, functor_map func next)
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

    let rec join : 'a t t -> 'a t = function
      | Nil -> Nil
      | Cons (Nil, outer) -> join outer
      | Cons (Cons (value, inner), outer) -> Cons (value, concatenate inner (join outer))
    ;;
  end

  (* We generate the rest of the bindings using a factory *)
  module Full = Factory.Monad (Base)

  (* We just need to populate the present module *)
  include Full
  include Base

  (* It's nice to have notation so let's add that too *)
  module Notation = Notation_factory.Monad (Full)

  let map_std_list (func : 'a -> 'b) (list : 'a list) : 'b t =
    let rec aux (func : 'a -> 'b) (list : 'a list) (acc : 'b t) : 'b t =
      match list with
      | [] -> reverse acc
      | head :: tail -> aux func tail (Cons (func head, acc))
    in
    aux func list Nil
  ;;

  let of_std_list (list : 'a list) : 'a t = map_std_list (fun a -> a) list

  module Specialize (Renderable : Typeclass.RENDERABLE) = struct
    include Full

    type t = Renderable.t Base.t

    let render (list : t) : string =
      let rec aux (list : t) (acc : string) : string =
        match list with
        | Nil -> acc
        | Cons (value, Nil) -> Renderable.render value
        | Cons (value, next) ->
          aux next (Printf.sprintf "%s ; %s" (Renderable.render value) acc)
      in
      "[" ^ aux list "" ^ "]"
    ;;
  end
end
