module type FIX_TYPE = functor (F : Typeclass.FUNCTOR) -> sig
  type fix = Fix of fix F.t

  val unfix : fix -> fix F.t
end

module Fix : FIX_TYPE =
functor
  (F : Typeclass.FUNCTOR)
  ->
  struct
    type fix = Fix of fix F.t

    let unfix (Fix f) : fix F.t = f
  end

module Loop = struct
  type ('a, 'b) t =
    | Base of 'a
    | Continue of 'b

  let return (value : 'a) : ('a, 'b) t = Base value
  let continue (value : 'b) : ('a, 'b) t = Continue value

  let rec loop (func : 'b -> ('a, 'b) t) (value : 'b) : 'a =
    match func value with
    | Base a -> a
    | Continue b' -> loop func b'
  ;;
end
