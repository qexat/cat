module type MAGMA = sig
  type t

  val compose : t -> t -> t
end

module type QUASIGROUP = sig
  include MAGMA

  val invert : t -> t
end

module type UNITAL_MAGMA = sig
  include MAGMA

  (** [id] must satisfy the following laws:
      - ∀(x : t), compose id x = x
      - ∀(x : t), compose x id = x *)
  val id : t
end

module type SEMIGROUP = sig
  (** Semigroups must satisfy the following law:
      - ∀(x y z : t), compose (compose x y) z = compose x (compose y z) *)

  include MAGMA
end

module type LOOP = sig
  include UNITAL_MAGMA
  include QUASIGROUP with type t := t
end

module type ASSOCIATIVE_QUASIGROUP = sig
  include QUASIGROUP
  include SEMIGROUP with type t := t
end

module type MONOID = sig
  include UNITAL_MAGMA
  include SEMIGROUP with type t := t
end

module type GROUP = sig
  include QUASIGROUP
  include UNITAL_MAGMA with type t := t
  include SEMIGROUP with type t := t
end

module type LATTICE = sig
  (** Lattices must satisfy the absorption laws:
      - ∀(a b : t), join a (meet a b) = a
      - ∀(a b : t), meet a (join a b) = a *)

  type t

  (** [meet] must be idempotent, commutative and associative *)
  val meet : t -> t -> t

  (** [join] must be idempotent, commutative and associative *)
  val join : t -> t -> t
end

module type BOUNDED_LATTICE = sig
  include LATTICE

  (** [top] is the identity of [meet] *)
  val top : t

  (** [bottom] is the identity of [join] *)
  val bottom : t
end

module type FUNCTOR = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type CONTRAVARIANT = sig
  type 'a t

  val map : ('b -> 'a) -> 'a t -> 'b t
end

module type APPLICATIVE = sig
  include FUNCTOR

  val lift : 'a -> 'a t
  val lift_binary : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end

module type SELECTIVE = sig
  include APPLICATIVE

  val select : ('a, 'b) Either.t t -> ('a -> 'b) t -> 'b t
end

module type MONAD = sig
  include SELECTIVE

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type BIFUNCTOR = sig
  type ('a, 'b) t

  val map : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
end

module type PROFUNCTOR = sig
  type ('a, 'b) t

  val map : ('c -> 'a) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
end

module type BICONTRAVARIANT = sig
  type ('a, 'b) t

  val map : ('c -> 'a) -> ('d -> 'b) -> ('a, 'b) t -> ('c, 'd) t
end

module type BIAPPLICATIVE = sig
  include BIFUNCTOR

  val lift : 'a -> 'b -> ('a, 'b) t

  val lift_binary
    :  ('a -> 'b -> 'c -> 'd -> 'e * 'f)
    -> ('a, 'b) t
    -> ('c, 'd) t
    -> ('e, 'f) t
end

module type DYAD = sig
  include BIAPPLICATIVE

  val bind : ('a, 'b) t -> ('a -> 'b -> ('c, 'd) t) -> ('c, 'd) t
end
