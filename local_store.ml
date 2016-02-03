open Utility
open Type

type t =
{
  locals : int IntMap.t;
  count : int;
  arguments_supplied : int;
}

let empty =
  { locals = IntMap.empty; count = 0; arguments_supplied = 0 }

let write_local local_store (Local local) value =
  let value = unsigned_word value in
  let locals = IntMap.add local value local_store.locals in
  { local_store with locals }

let read_local local_store (Local local) =
  IntMap.find local local_store.locals

let add local_store (Local n) default_value =
  let locals = IntMap.add n default_value local_store.locals in
  let count = max local_store.count n in
  { local_store with locals; count }

let create_default_locals story routine_address =
  let count = Routine.locals_count story routine_address in
  let rec aux acc i =
    if i > count then
      acc
    else
      let default_value = Routine.local_default_value story routine_address i in
      let new_store = add acc (Local i) default_value in
      aux new_store (i + 1) in
  aux empty 1

let display local_store =
  let to_string local value =
    Printf.sprintf "local%01x=%04x " (local - 1) value in
  let folder local value acc =
    acc ^ (to_string local value) in
  let locals = local_store.locals in
  IntMap.fold folder locals ""
