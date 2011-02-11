﻿namespace FSKit

open System

module Utils =
  let toTrue _ = true
  let toFalse _ = false

  let inline refEq (a:obj) (b:obj) = Object.ReferenceEquals(a, b)
  let inline refNotEq a b = refEq a b |> not

  let inline isNull (a:obj) = Object.ReferenceEquals(a, null)
  let inline notNull (a:obj) = Object.ReferenceEquals(a, null) |> not

  let inline isSubClass (a:Type) (b:Type) = b.IsSubclassOf a
  let inline isSubClassT<'a> (b:Type) = isSubClass typeof<'a> b

  let inline isType (a:Type) (b:Type) = refEq a b || isSubClass a b
  let inline isTypeT<'a> (b:Type) = isType typeof<'a> b

  let inline isVoid (t:Type) = refEq typeof<Void> t
  let inline isNaNOrInf (d:double) = Double.IsNaN d || Double.IsInfinity d