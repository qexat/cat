module Catamorphism (F : Typeclass.FUNCTOR) = struct
  module Fix = Iter.Fix (F)

  let rec cata (algebra : 'a Structure.F_algebra(F).t) (Fix.Fix f) =
    algebra (F.functor_map (cata algebra) f)
  ;;
end

module Anamorphism (F : Typeclass.FUNCTOR) = struct
  module Fix = Iter.Fix (F)

  let rec ana (coalgebra : 'a Structure.F_coalgebra(F).t) (value : 'a) : Fix.fix =
    Fix.Fix (F.functor_map (ana coalgebra) (coalgebra value))
  ;;
end

module Hylomorphism (F : Typeclass.FUNCTOR) = struct
  module Ana = Anamorphism (F)
  module Cata = Catamorphism (F)

  let hylo
        (algebra : 'a Structure.F_algebra(F).t)
        (coalgebra : 'a Structure.F_coalgebra(F).t)
        (value : 'a)
    : 'a
    =
    Cata.cata algebra (Ana.ana coalgebra value)
  ;;
end
