module type MinimalFunctor = sig
  type 'a t

  val functor_map : ('a -> 'b) -> 'a t -> 'b t
end

module type Functor = sig
  include MinimalFunctor

  val ( <$ ) : 'a -> 'b t -> 'a t
end

module type MinimalApplicative = sig
  include MinimalFunctor

  val lift : 'a -> 'a t
  val lift_binary : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end

module type Applicative = sig
  include MinimalApplicative
  include Functor with type 'a t := 'a t

  val distribute : ('a -> 'b) t -> 'a t -> 'b t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val ( <* ) : 'a t -> 'b t -> 'a t
end

module type MinimalMonad = sig
  include MinimalApplicative

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type Monad = sig
  include MinimalMonad
  include Applicative with type 'a t := 'a t
end

module Algebra (F : Functor) = struct
  type 'a t = 'a F.t -> 'a
end

module Coalgebra (F : Functor) = struct
  type 'a t = 'a -> 'a F.t
end

module Fix (F : Functor) = struct
  type t = Fix of t F.t

  let fix value = Fix value
  let unfix (Fix f) = f
end

module Catamorphism (F : Functor) = struct
  module FixF = Fix (F)

  let rec cata (alg : 'a Algebra(F).t) (FixF.Fix f) =
    alg (F.functor_map (cata alg) f)
end

module Anamorphism (F : Functor) = struct
  module FixF = Fix (F)

  let rec ana (coalg : 'a Coalgebra(F).t) value =
    FixF.fix (F.functor_map (ana coalg) (coalg value))
end

module Hylomorphism (F : Functor) = struct
  module AnaF = Anamorphism (F)
  module CataF = Catamorphism (F)

  let hylo (alg : 'a Algebra(F).t) (coalg : 'a Coalgebra(F).t) value =
    CataF.cata alg (AnaF.ana coalg value)
end
