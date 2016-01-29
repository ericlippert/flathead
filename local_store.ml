(* The local variables store knows
* Which locals were supplied from arguments
* How many locals there are
* Values of all locals *)

open Utility
open Iff
open Type

type t =
{
  locals : int IntMap.t;
  count : int;
  arguments_supplied : int;
}

(* TODO: Be more clever *)
let make locals count arguments_supplied =
  { locals; count; arguments_supplied }

let empty =
  { locals = IntMap.empty; count = 0; arguments_supplied = 0 }

let maximum_local = 15

let write_local local_store (Local local) value =
  if local < 0 || local > local_store.count then
    failwith (Printf.sprintf "write_local: local %d invalid; count is %d" local local_store.count)
  else
    let value = unsigned_word value in
    { local_store with locals = IntMap.add local value local_store.locals }

let read_local local_store (Local local) =
  if local < 0 || local > local_store.count then
    failwith (Printf.sprintf "read_local: local %d invalid; count is %d" local local_store.count)
  else
    IntMap.find local local_store.locals

(* Handy debugging methods *)
let display_locals local_store =
  let to_string local value =
    Printf.sprintf "local%01x=%04x " (local - 1) value in
  let folder local value acc =
    acc ^ (to_string local value) in
  let locals = local_store.locals in
  IntMap.fold folder locals ""

let make_locals_record local_store =
  let rec aux acc n =
    if n = 0 then
      acc
    else
      let local_value = read_local local_store (Local n) in
      aux ((Integer16 (Some local_value)) :: acc) (n - 1) in
  aux [] local_store.count

let make_locals_from_record arguments_supplied locals_list =
  let decode_int16 form =
    match form with
    | (Integer16 (Some v)) -> v
    | _ -> failwith "TODO handle failure reading locals" in
  let rec aux map i locs =
    match locs with
    | [] -> map
    | h :: t ->
      let v = decode_int16 h in
      let new_map = IntMap.add i v map in
      aux new_map (i + 1) t in
  let map = aux IntMap.empty 1 locals_list in
  { locals = map; count = List.length locals_list ; arguments_supplied }

let add local_store (Local n) default_value =
  let locals = IntMap.add n default_value local_store.locals in
  let count = max local_store.count n in
  { local_store with locals; count }

let create_default_locals story routine_address =
  let count = Routine.locals_count story routine_address in
  let rec aux acc i=
    if i > count then
      acc
    else
      let default_value = Routine.local_default_value story routine_address i in
      let new_store = add acc (Local i) default_value in
      aux new_store (i + 1) in
aux empty 1

let write_arguments local_store arguments =
  let rec aux acc args i =
    match args with
    | [] -> acc
    | arg :: tail ->
      if i > acc.count then
        acc
      else
        let new_store = write_local acc (Local i) arg in
        aux new_store tail (i + 1) in
  aux local_store arguments 1
