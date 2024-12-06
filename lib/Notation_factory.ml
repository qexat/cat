module Functor (F : Typeclass.FUNCTOR) = struct
  open F

  let ( <$ ) : 'a -> 'b t -> 'a t = F.const_map
end

module Applicative (A : Typeclass.APPLICATIVE) = struct
  open A

  let ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t = distribute
  let ( *> ) : 'a t -> 'b t -> 'b t = sequence
  let ( <* ) : 'a t -> 'b t -> 'a t = lifted_const
  let ( let+ ) (app : 'a t) (func : 'a -> 'b) : 'b t = functor_map func app

  let ( and+ ) (app1 : 'a t) (app2 : 'b t) : ('a * 'b) t =
    functor_map (fun a b -> a, b) app1 <*> app2
  ;;
end

module Monad (M : Typeclass.MONAD) = struct
  open M
  include Applicative (M)

  let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t = bind
  let ( let* ) (monad : 'a t) (func : 'a -> 'b t) : 'b t = monad >>= func
  let ( and* ) : 'a t -> 'b t -> ('a * 'b) t = ( and+ )
end
