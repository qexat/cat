module Functor (F : Typeclass.FUNCTOR_BASE) = struct
  include F

  let const_map (value : 'a) : 'b t -> 'a t = functor_map (Fun.const value)
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
