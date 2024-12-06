module type FUNCTOR_BASE = sig
  type 'a t

  val functor_map : ('a -> 'b) -> 'a t -> 'b t
end

module type FUNCTOR = sig
  include FUNCTOR_BASE

  val const_map : 'a -> 'b t -> 'a t
end

module type CONTRAVARIANT_FUNCTOR_BASE = sig
  type 'a t

  val contravariant_functor_map : ('a -> 'b) -> 'b t -> 'a t
end

module type CONTRAVARIANT_FUNCTOR = sig
  include CONTRAVARIANT_FUNCTOR_BASE

  val contravariant_const_map : 'b -> 'b t -> 'a t
end

module type BIFUNCTOR_BASE = sig
  type ('a, 'b) t

  val bifunctor_map : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
end

module type BIFUNCTOR = sig
  include BIFUNCTOR_BASE

  val functor_map_left : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
  val functor_map_right : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  val const_map : 'b -> 'd -> ('a, 'c) t -> ('b, 'd) t
  val const_map_left : 'b -> ('a, 'c) t -> ('b, 'c) t
  val const_map_right : 'c -> ('a, 'b) t -> ('a, 'c) t
end

module type APPLICATIVE_BASE = sig
  include FUNCTOR_BASE

  val lift : 'a -> 'a t
  val lift_binary : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end

module type APPLICATIVE = sig
  include APPLICATIVE_BASE
  include FUNCTOR with type 'a t := 'a t

  val distribute : ('a -> 'b) t -> 'a t -> 'b t
  val sequence : 'a t -> 'b t -> 'b t
  val lifted_const : 'a t -> 'b t -> 'a t
end

module type MONAD_BASE = sig
  include APPLICATIVE_BASE

  val join : 'a t t -> 'a t
end

module type MONAD = sig
  include MONAD_BASE
  include APPLICATIVE with type 'a t := 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type RENDERABLE = sig
  type t

  val render : t -> string
end
