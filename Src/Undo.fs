namespace FSKit

module Undo =

  (*
  //  open Undo.Operators
  //  
  //  let str = "foo bar" |> Undo.init
  //  let modified = str |~> (String.map (fun c -> int c + 1 |> char))
  //  
  //  let modifiedValue = !!modified
  //  let undoValue = modified |> Undo.undo |> Undo.value
  *)
    
  type History<'a> 
    = Empty
    | History of 'a * History<'a>

  let init value = History(value, Empty)

  let undo = function
    | Empty -> failwith "Can't undo empty history"
    | History(_, history) -> history

  let apply history f =
    match history with
    | Empty -> failwith "Can't apply function to empty history"
    | History(v, _) -> History(f v, history)

  let value = function
    | Empty -> failwith "Can't get value of empty history"
    | History(v, _) -> v

  let hasValue = function
    | Empty -> false
    | _ -> true

  let tryValue = function
    | Empty -> None
    | History(v, _) -> Some v

  let discard = function
    | Empty -> Empty
    | History(v, _) -> History(v, Empty)

  module Operators =
  
    let inline (!!?) history = tryValue history
    let inline (!!) history = value history
    let inline (|~>) history f = apply history f