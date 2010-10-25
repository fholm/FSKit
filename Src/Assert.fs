namespace FSKit

open Microsoft.VisualStudio.TestTools.UnitTesting

module Assert =

  let equal a b = Assert.AreEqual(a, b)
  let equalM a b (m:string) = Assert.AreEqual(a, b, m)

  let notEqual a b = Assert.AreNotEqual(a, b)
  let notEqualM a b (m:string) = Assert.AreNotEqual(a, b, m)

  let same a b = Assert.AreSame(a, b)
  let sameM a b (m:string) = Assert.AreSame(a, b, m)

  let notSame a b = Assert.AreNotSame(a, b)
  let notSameM a b (m:string) = Assert.AreNotSame(a, b, m)

  let is a b = Assert.IsInstanceOfType(a, b)
  let isM a b (m:string) = Assert.IsInstanceOfType(a, b, m)

  let isNot a b = Assert.IsNotInstanceOfType(a, b)
  let isNotM a b (m:string) = Assert.IsNotInstanceOfType(a, b, m)

  let isT<'a> v = is v typeof<'a>
  let isTM<'a> v m = isM v typeof<'a> m

  let isNotT<'a> v = isNot v typeof<'a>
  let isNotTM<'a> v m = isNotM v typeof<'a> m

  let isTrue v = Assert.IsTrue v
  let isTrueM v (m:string) = Assert.IsTrue(v, m)

  let isFalse v = Assert.IsFalse v
  let isFalseM v (m:string) = Assert.IsFalse(v, m)

  let isNull v = Assert.IsNull v
  let isNullM v m = Assert.IsNull(v, m)

  let isNotNull v = Assert.IsNotNull v
  let isNotNullM v m = Assert.IsNotNull(v, m)

  let fail m = Assert.Fail m
  let inconclusive m = Assert.Inconclusive m

