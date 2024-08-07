(** Various instances of the functorial typeclasses. *)

module MinimalOption = struct
  type 'a t = None | Some of 'a

  let functor_map func = function
    | None -> None
    | Some value -> Some (func value)

  let lift value = Some value

  let lift_binary func opt1 opt2 =
    match functor_map func opt1 with
    | Some rest -> functor_map rest opt2
    | None -> None

  let bind opt func = match opt with None -> None | Some value -> func value
end

module Option = struct
  module Minimal = MinimalOption
  module Complete = Factory.MonadFactory (Minimal)
  module Syntax = Factory.MonadSyntaxFactory (Complete)
  include Syntax
  include Complete
  include Minimal
end

(*
TODO: fix this when I'm less stupid
module MinimalLinkedList = struct
     type 'a t = Nil | Cons of 'a * 'a t

     let prepend value lst = Cons (value, lst)

     let rec concatenate lst1 lst2 =
       match lst1 with
       | Nil -> lst2
       | Cons (value, next) -> Cons (value, concatenate next lst2)

     let ( ++ ) = concatenate

     let rec flatten = function
       | Nil -> Nil
       | Cons (Cons (value, next1), next2) -> Cons (value, next1) ++ flatten next2
       | Cons (Nil, next2) -> flatten next2

     let rec functor_map func = function
       | Nil -> Nil
       | Cons (value, next) -> Cons (func value, functor_map func next)

     let lift value = Cons (value, Nil)

     let rec lift_a2 func lst1 lst2 =
       lift_a2 (fun a -> a) (functor_map func lst1) lst2

     let bind lst func = flatten (functor_map func lst)
   end

   module LinkedList = struct
     module Complete = Factory.MonadFactory (MinimalLinkedList)
     include Factory.MonadSyntaxFactory (Complete)
     include Complete
     include MinimalLinkedList

     let prepend value lst = Cons (value, lst)

     let rec concatenate lst1 lst2 =
       match lst1 with
       | Nil -> lst2
       | Cons (value, next) -> Cons (value, concatenate next lst2)

     let ( <$ ) = prepend
     let rec pp = function Nil -> "Nil" | Cons (_, next) -> "Cons -> " ^ pp next
   end *)

(* The following is a joke in the PLD (formerly r/PL) discord server*)

module MinimalMop = struct
  type 'a t = Clean | Dirty of 'a

  let functor_map func = function
    | Clean -> Clean
    | Dirty stuff -> Dirty (func stuff)

  let lift value = Dirty value
end

module Mop = struct
  module Minimal = MinimalMop
  include Factory.FunctorFactory (Minimal)
  include Minimal
end

module MinimalBucket = struct
  type 'a t = Empty | Full of 'a

  let functor_map func = function
    | Empty -> Empty
    | Full contents -> Full (func contents)

  let lift value = Full value
end

module Bucket = struct
  module Minimal = MinimalBucket
  include Factory.FunctorFactory (Minimal)
  include Minimal
end

module MinimalMod = struct
  type 'a t = Mod of ('a Mop.t * 'a Bucket.t)

  let functor_map func = function
    | Mod (mop, bucket) ->
        Mod (Mop.functor_map func mop, Bucket.functor_map func bucket)

  let lift value = Mod (Mop.lift value, Bucket.lift value)
end

module Mod = struct
  module Minimal = MinimalMod
  include Factory.FunctorFactory (Minimal)
  include Minimal
end
