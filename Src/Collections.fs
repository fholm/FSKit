namespace FSKit

open System
open System.Collections
open System.Collections.Generic

module Collections =

  module Queue =

    //  let q = 
    //    List.fold (
    //      fun queue item -> 
    //        queue |> Queue.enqueue item) Queue.empty [1..10]
    //  
    //  let qSize = Queue.size q // 10
    //  let first, q2 = q |> Queue.dequeue
    //  let q2Size = Queue.size q2 // 9
    
    type Queue<'a> = Queue of 'a list * 'a list

    let empty = Queue([], [])

    let enqueue item queue =
      match queue with
      | Queue(front, back) -> 
        Queue(front, item :: back)

    let rec dequeue queue = 
      match queue with
      | Queue([], []) -> failwith "Empty queue"
      | Queue([], back) -> Queue(back |> List.rev, []) |> dequeue
      | Queue(front, back) -> front|> List.head, Queue(front|> List.tail, back)

    let size = function
      | Queue(front, back) -> front.Length + back.Length

  module AvlTree =

    //  let avl =
    //    ['a'..'z'] |> List.fold (fun avl char ->
    //      AvlTree.insert char char avl
    //    ) AvlTree.empty
    //  
    //  avl |> AvlTree.min // 'z'
    //  avl |> AvlTree.max // 'a'
    //  avl |> AvlTree.exists 'b' // true
    //  avl |> AvlTree.find 'd' // Some 'd'
    //  avl |> AvlTree.size // 26
    //  avl |> AvlTree.value // 'p' (root node)
    //  
    //  let avl2 = avl |> AvlTree.delete 'd' // 
    //  avl2 |> AvlTree.exists 'd' // false
    //  avl2 |> AvlTree.size // 25

    open System.Collections
    open System.Collections.Generic

    type Node<'k, 'v> when 'k : comparison
      = Empty
      | Node of 'k * 'v * int * int * Node<'k, 'v> * Node<'k, 'v>
      with
        interface IEnumerable<'v> with
          member x.GetEnumerator() =
            let rec inOrder (tree:Node<'k, 'v>) =
              seq {
                match tree with
                | Empty -> yield! Seq.empty
                | Node(_, v, _, _, l, r) -> 
                  yield! inOrder l; yield v; yield! inOrder r
              }

            (inOrder x).GetEnumerator()

          member x.GetEnumerator() =
            (x :> IEnumerable<'v>).GetEnumerator() :> IEnumerator

    let empty = Empty
    
    let rec min = function
      | Empty -> failwith "Empty tree"
      | Node(_, v, _, _, Empty, _) -> v
      | Node(_, _, _, _, l, _) -> min l 

    let rec max = function
      | Empty -> failwith "Empty tree"
      | Node(_, v, _, _, _, Empty) -> v
      | Node(_, _, _, _, _, r) -> max r
      
    let left = function
      | Node(_, _, _, _, l, _) -> l
      | Empty -> failwith "Can't get left child of empty node"

    let right = function
      | Node(_, _, _, _, _, r) -> r
      | Empty -> failwith "Can't get right child of empty node"

    let value = function
      | Node(_, v, _, _, _, _) -> v
      | Empty -> failwith "Can't get value of empty node"

    let size = function
      | Empty -> 0
      | Node(_, _, s, _, _, _) -> s

    let private sizeOf l r = 
      (size l) + (size r) + 1

    let height = function
      | Empty -> 0
      | Node(_, _, _, h, _, _) -> h
      
    let private heightOf l r =
      (Microsoft.FSharp.Core.Operators.max (height l) (height r)) + 1

    let private rotateLeft root =
      match root with
      | Node(rk, rv, _, rh, rl, Node(pk, pv, _, ph, pl, pr)) ->
        let root = Node(rk, rv, sizeOf rl pl, heightOf rl pl, rl, pl)
        Node(pk, pv, sizeOf root pr, heightOf root pr, root, pr)
      
      | _ -> failwith "Can't rotate tree left"

    let private rotateRight root =
      match root with
      | Node(rk, rv, _, rh, Node(pk, pv, _, ph, pl, pr), rr) ->
        let root = Node(rk, rv, sizeOf pr rr, heightOf pr rr, pr, rr)
        Node(pk, pv, sizeOf root pl, heightOf root pl, pl, root)

      | _ -> failwith "Can't rotate tree right"

    let private balanceOf l r = 
      (height l) - (height r)

    let balance = function
      | Empty -> 0
      | Node(_, _, _, _, l, r) -> balanceOf l r

    let private rebalance k v l r =
      let h = heightOf l r 
      let s = sizeOf l r

      match balanceOf l r with
      | -2 ->
        rotateLeft 
         (match balance r with
          | 1 -> Node(k, v, s, h, l, rotateRight r)
          | _ -> Node(k, v, s, h, l, r))

      | 2 ->
        rotateRight
         (match balance l with
          | -1 -> Node(k, v, s, h, rotateLeft l, r)
          | _ -> Node(k, v, s, h, l, r))

      | _ -> Node(k, v, s, h, l, r)

    let inOrderPrev (tree:Node<'k, 'v>) =
      let rec prev prevVal tree = 
        match tree with
        | Empty -> Empty
        | Node(k, v, _, _, l, Empty) -> prevVal := Some(k, v); l
        | Node(k, v, s, h, l, r) -> 
          let r = prev prevVal r
          Node(k, v, sizeOf l r, h, l, r)
        
      let prevVal = ref None

      match tree with
      | Empty -> Empty, !prevVal
      | Node(_, _, _, _, l, _) -> prev prevVal l, !prevVal

    let inOrderNext (tree:Node<'k, 'v>) =
      let rec next nextVal tree = 
        match tree with
        | Empty -> Empty
        | Node(k, v, _, _, Empty, r) -> nextVal := Some(k, v); r
        | Node(k, v, s, h, l, r) -> 
          let l = next nextVal l
          Node(k, v, sizeOf l r, h, l, r)

      let nextVal = ref None

      match tree with
      | Empty -> Empty, !nextVal
      | Node(_, _, _, _, _, r) -> next nextVal r, !nextVal

    let rec find key (tree:Node<'k, 'v>) =
      match tree with
      | Empty -> None
      | Node(k, v, _, _, l, r) ->
        if key < k
          then find key l
          elif key > k
            then find key r
            else Some v

    let rec exists key (tree:Node<'k, 'v>) =
      tree |> find key |> Option.isSome

    let rec insert key value (tree:Node<'k, 'v>) =
      match tree with
      | Empty -> Node(key, value, 1, 1, Empty, Empty)
      | Node(k, v, s, h, l, r) -> 
        if key < k 
          then rebalance k v (insert key value l) r
          elif key > k 
            then rebalance k v l (insert key value r)
            else Node(key, value, s, h, l, r)

    let rec delete key (tree:Node<'k, 'v>) =
      match tree with
      | Empty -> Empty
      | Node(k, v, _, _, l, r) ->
        if key < k
          then rebalance k v (delete key l) r
          elif key > k
            then rebalance k v l (delete key r)
            else 
              match inOrderPrev tree with
              | _, None -> 
                match inOrderNext tree with
                | _, None -> Empty
                | r, Some(k, v) -> rebalance k v l r
              | l, Some(k, v) -> rebalance k v l r

    let rec preOrder (tree:Node<'k, 'v>) =
      seq {
        match tree with
        | Empty -> yield! Seq.empty
        | Node(_, v, _, _, l, r) ->
          yield v; yield! preOrder l; yield! preOrder r
      }

    let rec postOrder (tree:Node<'k, 'v>) =
      seq {
        match tree with
        | Empty -> yield! Seq.empty
        | Node(_, v, _, _, l, r) ->
          yield! postOrder l; yield! postOrder r; yield v
      }

    let levelOrder (tree:Node<'k, 'v>) =
      let queue = new Queue<Node<'k, 'v>>()
      tree |> queue.Enqueue

      seq {
        while queue.Count > 0 do
          let node = queue.Dequeue()
          yield node |> value
          node |> left |> queue.Enqueue 
          node |> right |> queue.Enqueue
      }

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