open Story
open Instruction
open Utility

let first_global = 16
let last_global = 255

(* Note that globals are indexed starting at 16 *)
let read story (Global global_number) =
  if global_number < first_global || global_number > last_global then
    failwith "global variable index out of range"
  else
    let base = global_variables_table_base story in
    let offset = (global_number - first_global) * 2 in
    read_word story (base + offset)

let display story =
  let to_string g =
    Printf.sprintf "%02x %04x\n" (g - first_global) (read story (Global g)) in
  accumulate_strings_loop to_string first_global (last_global + 1)

let write story (Global global_number) value =
  if global_number < first_global || global_number > last_global then
      failwith "global variable index out of range"
  else
    let base = global_variables_table_base story in
    let offset = (global_number - first_global) * 2 in
    write_word story (base + offset) value
