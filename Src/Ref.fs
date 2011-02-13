namespace FSKit

module Ref =

  let inline incr64 i = i := !i + 1L; !i
  let inline decr64 i = i := !i - 1L; !i
  let inline incru64 i = i := !i + 1UL; !i
  let inline decru64 i = i := !i - 1UL; !i
  
  let inline incru i = i := !i + 1u; !i
  let inline decru i = i := !i - 1u; !i

  let inline incr16 i = i := !i + 1s; !i
  let inline decr16 i = i := !i - 1s; !i
  let inline incru16 i = i := !i + 1us; !i
  let inline decru16 i = i := !i - 1us; !i

  let inline incr8 i = i := !i + 1y; !i
  let inline decr8 i = i := !i - 1y; !i
  let inline incru8 i = i := !i + 1uy; !i
  let inline decru8 i = i := !i - 1uy; !i
  
  let inline incrf i = i := !i + 1.0f; !i
  let inline decrf i = i := !i - 1.0f; !i
  let inline incrd i = i := !i + 1.0; !i
  let inline decrd i = i := !i - 1.0; !i

  let inline update f r = r := f !r
