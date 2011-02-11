namespace FSKit

module Seq =

  let first seq' = Seq.find Utils.toTrue seq'
  let tryFirst seq' = Seq.tryFind Utils.toTrue seq'

  //Author: http://fssnip.net/18
  let rec iterate value f = 
    seq { 
      yield value
      yield! iterate (f value) f
    }

  //Author: http://fssnip.net/1K
  let tryTake (n:int) (s:seq<_>) =
    let e = s.GetEnumerator ()
    let i = ref 0
    seq {
      while e.MoveNext () && !i < n do
        i := !i + 1
        yield e.Current
    }

  //Author: http://fssnip.net/2n
  let unsort xs =
      let rand = new System.Random(Seed=0)
      xs |> Seq.map (fun x -> rand.Next(),x)
         |> Seq.cache
         |> Seq.sortBy fst
         |> Seq.map snd

  //Author: http://fssnip.net/1R
  let everyNth n (input:seq<_>) = 
    seq { 
      use en = input.GetEnumerator()

      let rec nextN n = 
        if n = 0 then true else en.MoveNext() && (nextN (n - 1)) 

      while nextN n do
        yield en.Current 
    }
