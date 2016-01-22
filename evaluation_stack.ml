open Utility
open Iff

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
  let folder acc item =
    acc ^ (to_string item) in
  let items =  stack.items in
  List.fold_left folder "" items

let make_stack_records stack =
  let rec aux acc items =
    match items with
    | [] -> acc
    | h :: t -> aux ((Integer16 (Some (h))) :: acc) t in
  List.rev (aux [] stack.items)

let make_stack_from_record records =
  let decode_int16 form =
    match form with
    | (Integer16 (Some v)) -> v
    | _ -> failwith "TODO handle failure reading evaluation stack" in
  { items = List.rev (List.map decode_int16 records) }
