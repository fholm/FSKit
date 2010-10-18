namespace FSKit

open System.Diagnostics

module Perf =
  
  let time name f =
    let sw = new Stopwatch()
    sw.Start()
    f()
    sw.Stop()
    printfn "%s: %dms" name sw.ElapsedMilliseconds

  type Foo() =
    static member doStuff (i1, i2) =
      let y = i1 + i2
      let x = y + i1
      let z = x + y + i2
      z + x + y + i1