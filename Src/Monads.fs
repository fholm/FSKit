namespace FSKit

module Monads =

  type MaybeBuilder() =
    member x.Bind(v, f) = match v with Some v -> f v | _ -> None
    member x.Return v = Some v
    member x.ReturnFrom v = v
    member x.Delay f = f()

  let maybe = new MaybeBuilder()