module type FIX = functor (F : Typeclass.FUNCTOR) -> sig
  type fix = Fix of fix F.t

  val unfix : fix -> fix F.t
end

module Fix : FIX =
functor
  (F : Typeclass.FUNCTOR)
  ->
  struct
    type fix = Fix of fix F.t

    let unfix (Fix f) : fix F.t = f
  end

module Fix_tag (F : Typeclass.FUNCTOR) = struct
  type 't fix =
    | Empty
    | Tagged of ('t * 't fix F.t)

  let unfix : 't fix -> 't option = function
    | Empty -> None
    | Tagged (tag, _) -> Some tag
  ;;

  let untag : 't fix -> 't fix F.t option = function
    | Empty -> None
    | Tagged (_, rest) -> Some rest
  ;;
end
