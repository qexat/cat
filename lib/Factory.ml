module Functor (F : Typeclass.FUNCTOR_BASE) = struct
  include F

  let const_map (value : 'a) : 'b t -> 'a t = functor_map (Fun.const value)
end

module Contravariant_functor (CF : Typeclass.CONTRAVARIANT_FUNCTOR_BASE) = struct
  include CF

  let const_map (value : 'b) : 'b t -> 'a t = value |> Fun.const |> functor_map
end

module Bifunctor (BF : Typeclass.BIFUNCTOR_BASE) = struct
  include BF

  let functor_map_left (func : 'a -> 'b) : ('a, 'c) t -> ('b, 'c) t =
    functor_map func Fun.id
  ;;

  let functor_map_right (func : 'b -> 'c) : ('a, 'b) t -> ('a, 'c) t =
    functor_map Fun.id func
  ;;

  let const_map (left : 'b) (right : 'd) : ('a, 'c) t -> ('b, 'd) t =
    functor_map (Fun.const left) (Fun.const right)
  ;;

  let const_map_left (value : 'b) : ('a, 'c) t -> ('b, 'c) t =
    functor_map (Fun.const value) Fun.id
  ;;

  let const_map_right (value : 'c) : ('a, 'b) t -> ('a, 'c) t =
    functor_map Fun.id (Fun.const value)
  ;;
end

module Applicative (A : Typeclass.APPLICATIVE_BASE) = struct
  (* Functor part of A
     We do not require A to be a full-fledged functor as we can
     derive the additional methods ourselves.
  *)
  module Functor = Functor (A)
  include Functor

  (* Baseline of an applicative - its minimal requirements *)
  include A

  let distribute (func : ('a -> 'b) t) (value : 'a t) : 'b t =
    lift_binary (fun a -> a) func value
  ;;

  let sequence (app1 : 'a t) (app2 : 'b t) : 'b t =
    distribute (const_map (fun a -> a) app1) app2
  ;;

  let lifted_const (app1 : 'a t) (app2 : 'b t) : 'a t = lift_binary Fun.const app1 app2
end

module Monad (M : Typeclass.MONAD_BASE) = struct
  include M
  include Applicative (M)

  let bind (monad : 'a t) (func : 'a -> 'b t) : 'b t = join (functor_map func monad)
end
