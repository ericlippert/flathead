(* The local variables store knows
* Which locals were supplied from arguments
* How many locals there are
* Values of all locals *)

open Utility
open Iff

type t =
{
  locals : int IntMap.t;
  count : int;
  arguments_supplied : int;
}

(* TODO: Be more clever *)
let make locals count arguments_supplied =
  { locals; count; arguments_supplied }

let maximum_local = 15

let write_local local_store local value =
  if local < 0 || local > local_store.count then
    failwith (Printf.sprintf "write_local: local %d invalid; count is %d" local local_store.count)
  else
    let value = unsigned_word value in
    { local_store with locals = IntMap.add local value local_store.locals }

let read_local local_store local =
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
      aux ((Integer16 (Some (read_local local_store n))) :: acc) (n - 1) in
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
