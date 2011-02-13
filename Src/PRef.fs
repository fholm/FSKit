namespace FSKit

module PRef = 
  type RO = RO
  type RW = RW

  type t<'a, 'b>(r:'a ref) = 
    member internal x.Ref = r

  let pref r = t<_, RW>(ref r)
  let pref_ref r = t<_, RW>(r)

  module Operators = 
    let (!!) (r:t<_, _>) = !r.Ref
    let (=<) (r:t<_, RW>) v = r.Ref := v

  open Operators

  let get r = !!r
  let set r v = r =< v
  let ro (r:t<_, RW>) = t<_, RO>(r.Ref)