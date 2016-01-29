open Utility
open Type

let first_global = 16
let last_global = 255

(* Note that globals are indexed starting at 16 *)
let read story (Global global) =
  if global < first_global || global > last_global then
    failwith "global variable index out of range"
  else
    let (Global_table_base base) = Story.global_variables_table_base story in
    let offset = (global - first_global) * 2 in
    Story.read_word story (Word_address (base + offset))

let display story =
  let to_string g =
    Printf.sprintf "%02x %04x\n" (g - first_global) (read story (Global g)) in
  accumulate_strings_loop to_string first_global (last_global + 1)

let write story (Global global) value =
  if global < first_global || global > last_global then
      failwith "global variable index out of range"
  else
    let (Global_table_base base) = Story.global_variables_table_base story in
    let offset = (global - first_global) * 2 in
    Story.write_word story (Word_address (base + offset)) value
