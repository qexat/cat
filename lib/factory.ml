module FunctorFactory (F : Base.MinimalFunctor) = struct
  include F

  let ( <$ ) value = functor_map (Fun.const value)
end

module ApplicativeFactory (A : Base.MinimalApplicative) = struct
  include A
  include FunctorFactory (A)

  let distribute func value = lift_binary (fun a -> a) func value
  let ( *> ) app1 app2 = distribute ((fun a -> a) <$ app1) app2
  let ( <* ) app1 app2 = lift_binary Fun.const app1 app2
  let ( let+ ) opt func = functor_map func opt
  let ( and+ ) app1 app2 = distribute (functor_map Common.pair app1) app2
end

module MonadFactory (M : Base.MinimalMonad) = struct
  include M
  include ApplicativeFactory (M)
end

module ApplicativeSyntaxFactory (A : Base.Applicative) = struct
  include A

  let ( <*> ) = distribute
  let ( let+ ) app func = functor_map func app
  let ( and+ ) app1 app2 = functor_map Common.pair app1 <*> app2
end

module MonadSyntaxFactory (M : Base.Monad) = struct
  include M
  include ApplicativeSyntaxFactory (M)

  let ( >>= ) = bind
  let ( let* ) monad func = monad >>= func
  let ( and* ) = ( and+ )
end
