type 'a list =
  | Nil
  | Cons of ('a * 'a list)

type 'a option =
  | None
  | Some of 'a
