namespace FSKit

module Operators =
  let inline (|?) a b = match a with Some a -> a | _ -> b()

