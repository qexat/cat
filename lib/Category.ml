(* TODO: encode laws *)
module type CATEGORY = sig
  type obj

  type arrow =
    | Monadic of (obj -> obj)
    | Variadic of (obj -> arrow)

  val id : arrow
  val ( * ) : arrow -> arrow -> arrow
end

module type FUNCTOR = functor (_ : CATEGORY) -> CATEGORY
module type ENDOFUNCTOR = functor (C : CATEGORY) -> module type of C
