module Functor_mixin (F : Typeclass.FUNCTOR_BASE) = struct
  type 'a t = 'a F.t

  let const_map (value : 'a) : 'b t -> 'a t = F.functor_map (Fun.const value)
end

module Functor (F : Typeclass.FUNCTOR_BASE) = struct
  include F
  include Functor_mixin (F)
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

module Applicative_mixin (A : Typeclass.APPLICATIVE_BASE) = struct
  type 'a t = 'a A.t

  include (Functor_mixin (A) : Typeclass.FUNCTOR_MIXIN with type 'a t := 'a t)

  let distribute (func : ('a -> 'b) A.t) (value : 'a A.t) : 'b A.t =
    A.lift_binary (fun a -> a) func value
  ;;

  let sequence (app1 : 'a A.t) (app2 : 'b A.t) : 'b A.t =
    distribute (const_map (fun a -> a) app1) app2
  ;;

  let lifted_const (app1 : 'a A.t) (app2 : 'b A.t) : 'a A.t =
    A.lift_binary Fun.const app1 app2
  ;;
end

module Applicative (A : Typeclass.APPLICATIVE_BASE) = struct
  include A
  include Applicative_mixin (A)
end

module Monad_mixin (M : Typeclass.MONAD_BASE) = struct
  type 'a t = 'a M.t

  include (Applicative_mixin (M) : Typeclass.APPLICATIVE_MIXIN with type 'a t := 'a t)

  let bind (monad : 'a t) (func : 'a -> 'b t) : 'b t = M.join (M.functor_map func monad)
end

module Monad (M : Typeclass.MONAD_BASE) = struct
  include M
  include Monad_mixin (M)
end
