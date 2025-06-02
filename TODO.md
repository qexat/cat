# TODOs

- [x] Better names for contravariant^[ `contravariant_functor_map` can probably just be `functor_map` since the only way both can coexist is if `'a` and `'b` are equal - and which case they would be isomorphic] and bi-functor^[`bifunctor_map` can be `functor_map` since a bifunctor instance cannot be a functor at the same time, and it would be more consistent with bifunctor mixins] functions
- [x] Add more typeclasses^[Since bifunctors were added, we could have dyads (and convert `Either` to that as such)]
- [ ] Add more instances^[There is currently no instance for just `FUNCTOR`, also `Either` is pretty weak as a `BIFUNCTOR` instance because it is actually a dyad]
- [ ] Actually test stuff (i'm not even sure half the functions in `Number` work)
- [ ] Add more category and group theories stuff!
