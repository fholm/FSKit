namespace FSKit

open System

module Collections =

  module BinaryTree = 

    type Node<'k, 'v> when 'k : comparison
      = Empty
      | Node of 'k * 'v * Node<'k, 'v> * Node<'k, 'v>

    let empty = Empty

    let rec find key (tree:Node<'k, 'v>) =
      match tree with
      | Empty -> None
      | Node(k, v, l, r) ->
        if key < k
          then find key l
          elif key > k
            then find key r
            else Some v

    let rec exists key (tree:Node<'k, 'v>) =
      tree |> find key |> Option.isSome

    let rec insert key value (tree:Node<'k, 'v>) =
      match tree with
      | Empty -> Node(key, value, Empty, Empty)
      | Node(k, v, l, r) ->
        if key < k
          then Node(k, v, insert key value l, r)
          elif key > k
            then Node(k, v, l, insert key value r)
            else tree

  module AvlTree =
  
    type Node<'k, 'v> when 'k : comparison
      = Empty
      | Node of 'k * 'v * int * Node<'k, 'v> * Node<'k, 'v>

    let empty = Empty

    let rec find key (tree:Node<'k, 'v>) =
      match tree with
      | Empty -> None
      | Node(k, v, _, l, r) ->
        if key < k
          then find key l
          elif key > k
            then find key r
            else Some v

    let rec exists key (tree:Node<'k, 'v>) =
      tree |> find key |> Option.isSome

    let height n =
      match n with
      | Empty -> 0
      | Node(_, _, h, _, _) -> h

    let heightOf l r =
      (max (height l) (height r)) + 1

    let balanceOf n =
      match n with
      | Empty -> 0
      | Node(_, _, _, l, r) -> (height l) - (height r)

    let rotateLeft root =
      match root with
      | Node(rk, rv, rh, rl, Node(pk, pv, ph, pl, pr)) ->
        let root = Node(rk, rv, heightOf rl pl, rl, pl)
        Node(pk, pv, heightOf root pr, root, pr)
      
      | _ -> failwith "Can't rotate tree"

    let rotateRight root =
      match root with
      | Node(rk, rv, rh, Node(pk, pv, ph, pl, pr), rr) ->
        let root = Node(rk, rv, heightOf pr rr, pr, rr)
        Node(pk, pv, heightOf root pl, pl, root)
      | _ -> failwith "Can't rotate tree"

    let balanceInsert k v l r =
      let h = heightOf l r 
      let n = Node(k, v, h, l, r)

      match balanceOf n with
      | -2 ->
        let n = 
          match balanceOf r with
          | 1 -> Node(k, v, h, l, rotateRight r)
          | _ -> n

        rotateLeft n

      | 2 ->
        let n = 
          match balanceOf l with
          | -1 -> Node(k, v, h, rotateLeft l, r)
          | _ -> n

        rotateRight n

      | _ -> n

    let rec insert key value (tree:Node<'k, 'v>) =
      match tree with
      | Empty -> Node(key, value, 1, Empty, Empty)
      | Node(k, v, h, l, r) -> 
        if key < k 
          then balanceInsert k v (insert key value l) r
          elif key > k 
            then balanceInsert k v l (insert key value r)
            else Node(key, value, h, l, r)

  let mutable avl : AvlTree.Node<int, int> = AvlTree.empty
  let mutable map : Map<int, int> = Map.empty

  for i = 0 to 100000 do
    avl <- AvlTree.insert i i avl  

  for i = 0 to 100000 do
    map <- Map.add i i map

  for i = 0 to 100000 do
    AvlTree.exists i avl

  for i = 0 to 100000 do
    Map.tryFind i map

  //----------------------------------------------------------------------------
  type CopyOnWriteArray<'a>(storage:'a array) =
    member x.Length = storage.Length
    member x.Item index = storage.[index]
    
    new (size:int) = 
      CopyOnWriteArray(Array.zeroCreate size)

    member x.SetValue(index, value) =
      let size = if index >= storage.Length then index+1 else storage.Length
      let copy = Array.zeroCreate size
      System.Array.Copy(storage, copy, storage.Length)
      copy.[index] <- value
      CopyOnWriteArray<'a> copy

    member x.Tail =
      if storage.Length = 0 then None
      elif storage.Length = 1 then Some(CopyOnWriteArray<'a>(0))
      else
        let size = storage.Length - 1
        let copy = Array.zeroCreate size
        System.Array.Copy(storage, 1, copy, 0, size)
        Some(CopyOnWriteArray<'a> copy)
        
    member x.Head = if storage.Length = 0 then None else Some storage.[0]
    member x.Match = x.Head, x.Tail
    member x.Append value = x.SetValue(storage.Length, value)
    member x.Cons value =
      let size = storage.Length + 1
      let copy = Array.zeroCreate size
      copy.[0] <- value
      System.Array.Copy(storage, 0, copy, 1, storage.Length)
      CopyOnWriteArray<'a> copy

    interface System.Collections.Generic.IEnumerable<'a> with
      member x.GetEnumerator() = storage.GetEnumerator()
      member x.GetEnumerator() = 
        (seq{for a in storage do yield a}).GetEnumerator()
        
  //----------------------------------------------------------------------------
  module Operators = 

    let (+<) value (array:CopyOnWriteArray<_>) = array.Cons value
    let (>+) (array:CopyOnWriteArray<_>) value = array.Append value

  //----------------------------------------------------------------------------
  module Operations =

    module CopyOnWriteArray =

      let zeroCreate<'a> (size:int) = CopyOnWriteArray<'a> size
      let length (array:CopyOnWriteArray<_>) = array.Length
      let head (array:CopyOnWriteArray<_>) = array.Head
      let tail (array:CopyOnWriteArray<_>) = array.Tail
      let cons value (array:CopyOnWriteArray<_>) = array.Cons value
      let append (array:CopyOnWriteArray<_>) value = array.Append value
      let get index (array:CopyOnWriteArray<_>) = array.[index]
      let set index value (array:CopyOnWriteArray<_>) = 
        array.SetValue(index, value)

      let (|Head|_|) (array:CopyOnWriteArray<_>) = array.Head
      let (|Tail|_|) (array:CopyOnWriteArray<_>) = array.Tail