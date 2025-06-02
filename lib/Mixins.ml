module Functor (F : Typeclass.FUNCTOR) = struct
  include F

  let const_map (value : 'a) : 'b t -> 'a t = map (Fun.const value)
end

module Contravariant (F : Typeclass.CONTRAVARIANT) = struct
  include F

  let const_map (value : 'b) : 'b t -> 'a t = map (Fun.const value)
end

module Applicative (A : Typeclass.APPLICATIVE) = struct
  include Functor (A)
  include A

  let distribute (func : ('a -> 'b) t) (value : 'a t) : 'b t =
    lift_binary Fun.id func value
  ;;

  let sequence (app1 : 'a t) (app2 : 'b t) : 'b t =
    distribute (const_map Fun.id app1) app2
  ;;

  let lifted_const (app1 : 'a t) (app2 : 'b t) : 'a t = lift_binary Fun.const app1 app2
end

module Selective (S : Typeclass.SELECTIVE) = struct
  include Applicative (S)
  include S

  let branch (type a b c) (x : (a, b) Either.t t) (l : (a -> c) t) (r : (b -> c) t) : c t =
    let ( <*? ) = select in
    map
      (function
        | Either.Left left' -> Either.Left left'
        | Either.Right right' -> Either.Right (Either.Left right'))
      x
    <*? map (fun f x -> Either.Right (f x)) l
    <*? r
  ;;
end

module Monad (M : Typeclass.MONAD) = struct
  include Applicative (M)
  include M

  let join (monad : 'a t t) : 'a t = bind monad Fun.id
end

module Bifunctor (F : Typeclass.BIFUNCTOR) = struct
  include F

  let map_left (func : 'a -> 'b) : ('a, 'c) t -> ('b, 'c) t = map func Fun.id
  let map_right (func : 'b -> 'c) : ('a, 'b) t -> ('a, 'c) t = map Fun.id func

  let const_map (left : 'b) (right : 'd) : ('a, 'c) t -> ('b, 'd) t =
    map (Fun.const left) (Fun.const right)
  ;;

  let const_map_left (value : 'b) : ('a, 'c) t -> ('b, 'c) t =
    map (Fun.const value) Fun.id
  ;;

  let const_map_right (value : 'c) : ('a, 'b) t -> ('a, 'c) t =
    map Fun.id (Fun.const value)
  ;;
end
