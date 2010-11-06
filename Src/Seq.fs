namespace FSKit

module Seq =

  let first seq' = Seq.find (fun _ -> true) seq'
  let any seq' = Seq.tryFind (fun _ -> true) seq'