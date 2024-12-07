module type FUNCTOR_BASE = sig
  type 'a t

  val functor_map : ('a -> 'b) -> 'a t -> 'b t
end

module type FUNCTOR_MIXIN = sig
  type 'a t

  val const_map : 'a -> 'b t -> 'a t
end

module type FUNCTOR = sig
  type 'a t

  include FUNCTOR_BASE with type 'a t := 'a t
  include FUNCTOR_MIXIN with type 'a t := 'a t
end

module type CONTRAVARIANT_FUNCTOR_BASE = sig
  type 'a t

  val functor_map : ('a -> 'b) -> 'b t -> 'a t
end

module type CONTRAVARIANT_FUNCTOR_MIXIN = sig
  type 'a t

  val const_map : 'b -> 'b t -> 'a t
end

module type CONTRAVARIANT_FUNCTOR = sig
  type 'a t

  include CONTRAVARIANT_FUNCTOR_BASE with type 'a t := 'a t
  include CONTRAVARIANT_FUNCTOR_MIXIN with type 'a t := 'a t
end

module type BIFUNCTOR_BASE = sig
  type ('a, 'b) t

  val functor_map : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
end

module type BIFUNCTOR_MIXIN = sig
  type ('a, 'b) t

  val functor_map_left : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
  val functor_map_right : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  val const_map : 'b -> 'd -> ('a, 'c) t -> ('b, 'd) t
  val const_map_left : 'b -> ('a, 'c) t -> ('b, 'c) t
  val const_map_right : 'c -> ('a, 'b) t -> ('a, 'c) t
end

module type BIFUNCTOR = sig
  type ('a, 'b) t

  include BIFUNCTOR_BASE with type ('a, 'b) t := ('a, 'b) t
  include BIFUNCTOR_MIXIN with type ('a, 'b) t := ('a, 'b) t
end

module type APPLICATIVE_BASE = sig
  type 'a t

  include FUNCTOR_BASE with type 'a t := 'a t

  val lift : 'a -> 'a t
  val lift_binary : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end

module type APPLICATIVE_MIXIN = sig
  type 'a t

  include FUNCTOR_MIXIN with type 'a t := 'a t

  val distribute : ('a -> 'b) t -> 'a t -> 'b t
  val sequence : 'a t -> 'b t -> 'b t
  val lifted_const : 'a t -> 'b t -> 'a t
end

module type APPLICATIVE = sig
  type 'a t

  include APPLICATIVE_BASE with type 'a t := 'a t
  include FUNCTOR_MIXIN with type 'a t := 'a t
  include APPLICATIVE_MIXIN with type 'a t := 'a t
end

module type MONAD_BASE = sig
  type 'a t

  include APPLICATIVE_BASE with type 'a t := 'a t

  val join : 'a t t -> 'a t
end

module type MONAD_MIXIN = sig
  type 'a t

  include APPLICATIVE_MIXIN with type 'a t := 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type MONAD = sig
  type 'a t

  include MONAD_BASE with type 'a t := 'a t
  include APPLICATIVE_MIXIN with type 'a t := 'a t
  include MONAD_MIXIN with type 'a t := 'a t
end

module type RENDERABLE = sig
  type t

  val render : t -> string
end
