open Utility

type t =
{
  items : int list
}

let empty = { items = [] }

let length stack =
  List.length stack.items

let peek stack =
  match stack.items with
  | [] -> failwith "peeking an empty stack"
  | h :: _ -> h

let pop stack  =
  match stack.items with
  | [] -> failwith "popping empty stack"
  | _ :: t -> { items = t }

let push stack item =
  let item = unsigned_word item in
  { items = item :: stack.items }

let display_stack stack =
  let to_string item =
    Printf.sprintf " %04x" item in
  accumulate_strings to_string stack.items
