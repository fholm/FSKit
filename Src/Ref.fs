namespace FSKit

module Ref =

  let incr64 i = i := !i + 1L; !i
  let decr64 i = i := !i - 1L; !i
  let incru64 i = i := !i + 1UL; !i
  let decru64 i = i := !i - 1UL; !i

