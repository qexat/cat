module type NatFunctorType = sig
  type 'a t = Z | S of 'a

  include Base.Functor with type 'a t := 'a t
end

module NatFunctor : NatFunctorType = struct
  type 'a t = Z | S of 'a

  let functor_map func = function Z -> Z | S value -> S (func value)
  let ( <$ ) value = functor_map (Fun.const value)
end

module Nat = struct
  include Base.Fix (NatFunctor)
  module Catamorphism = Base.Catamorphism (NatFunctor)

  let z = fix NatFunctor.Z
  let s n = fix (NatFunctor.S n)

  let add a =
    let phi = function NatFunctor.Z -> a | NatFunctor.S b -> s b in
    Catamorphism.cata phi

  let ( + ) = add
end

let rec int_of_nat : Nat.t -> int = function
  | Nat.Fix NatFunctor.Z -> 0
  | Nat.Fix (NatFunctor.S n) -> 1 + int_of_nat n

let rec int_to_nat : int -> Nat.t = function
  | 0 -> Nat.Fix NatFunctor.Z
  | n -> Nat.Fix (NatFunctor.S (int_to_nat (n - 1)))
