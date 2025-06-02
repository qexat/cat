module Functor (F : Typeclass.FUNCTOR) = struct
  open Mixins.Functor (F)

  let ( <$ ) : 'a -> 'b t -> 'a t = const_map
end

module Contravariant (F : Typeclass.CONTRAVARIANT) = struct
  open Mixins.Contravariant (F)

  let ( >$ ) : 'b -> 'b t -> 'a t = const_map
end

module Applicative (A : Typeclass.APPLICATIVE) = struct
  open Mixins.Applicative (A)

  let ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t = distribute
  let ( *> ) : 'a t -> 'b t -> 'b t = sequence
  let ( <* ) : 'a t -> 'b t -> 'a t = lifted_const
  let ( let+ ) (app : 'a t) (func : 'a -> 'b) : 'b t = map func app

  let ( and+ ) (app1 : 'a t) (app2 : 'b t) : ('a * 'b) t =
    map (fun a b -> a, b) app1 <*> app2
  ;;
end

module Selective (S : Typeclass.SELECTIVE) = struct
  let ( <*? ) = S.select
end

module Monad (M : Typeclass.MONAD) = struct
  open Mixins.Monad (M)
  include Applicative (Mixins.Monad (M))

  let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t = bind
  let ( let* ) (monad : 'a t) (func : 'a -> 'b t) : 'b t = monad >>= func
  let ( and* ) : 'a t -> 'b t -> ('a * 'b) t = ( and+ )
end
