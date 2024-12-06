include Instance
include Number

let render (type t) (value : t) (module M : Typeclass.RENDERABLE with type t = t) : string
  =
  M.render value
;;
