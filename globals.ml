open Utility
open Type

let first_global = 16
let last_global = 255

let global_addr story (Global global) = 
  if global < first_global || global > last_global then
      failwith "global variable index out of range"
  else
    let (Global_table_base base) = Story.global_variables_table_base story in
    let base = Word_address base in
    let offset = global - first_global in
    inc_word_addr_by base offset

let read story global =
    Story.read_word story (global_addr story global)

let write story global value =
    Story.write_word story (global_addr story global) value
    
let display story =
  let to_string g =
    Printf.sprintf "%02x %04x\n" (g - first_global) (read story (Global g)) in
  accumulate_strings_loop to_string first_global (last_global + 1)

