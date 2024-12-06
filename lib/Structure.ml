module F_algebra (F : Typeclass.FUNCTOR) = struct
  type 'a t = 'a F.t -> 'a
end

module F_coalgebra (F : Typeclass.FUNCTOR) = struct
  type 'a t = 'a -> 'a F.t
end
