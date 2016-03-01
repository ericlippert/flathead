open Utility
open Type

type t =
{
  dynamic_memory : Immutable_bytes.t;
  static_memory : string;
}

let make dynamic static =
{
    dynamic_memory = Immutable_bytes.make dynamic;
    static_memory = static;
}

let read_byte story address =
  let dynamic_size = Immutable_bytes.size story.dynamic_memory in
  if is_in_range address dynamic_size then
    Immutable_bytes.read_byte story.dynamic_memory address
  else
    let static_addr = dec_byte_addr_by address dynamic_size in
    dereference_string static_addr story.static_memory
      
let read_word story address =
  let high = read_byte story (address_of_high_byte address) in
  let low = read_byte story (address_of_low_byte address) in
  256 * high + low

let write_byte story address value =
  let dynamic_memory = Immutable_bytes.write_byte story.dynamic_memory address value in
  { story with dynamic_memory }

let write_word story address value =
  let high = (value lsr 8) land 0xFF in
  let low = value land 0xFF in
  let story = write_byte story (address_of_high_byte address) high in
  write_byte story (address_of_low_byte address) low
  
let read_bit story address bit =
  fetch_bit bit (read_byte story address)

let read_word_bit story address bit =
  fetch_bit bit (read_word story address)

let write_set_bit story address bit =
  let orig_byte = read_byte story address in
  let new_byte = set_bit bit orig_byte in
  write_byte story address new_byte

let write_clear_bit story address bit =
  let orig_byte = read_byte story address in
  let new_byte = clear_bit bit orig_byte in
  write_byte story address new_byte

let write_set_bit_to story address bit value =
  let orig_byte = read_byte story address in
  let new_byte = set_bit_to bit orig_byte value in
  write_byte story address new_byte

let write_set_word_bit_to story address bit value =
  let orig_word = read_word story address in
  let new_word = set_bit_to bit orig_word value in
  write_word story address new_word

let header_size = 64

let version_offset = Byte_address 0
let version story =
  match read_byte story version_offset with
  | 1 -> V1
  | 2 -> V2
  | 3 -> V3
  | 4 -> V4
  | 5 -> V5
  | 6 -> V6
  | 7 -> V7
  | 8 -> V8
  | _ -> failwith "unknown version"
  
let v5_or_lower v =
  match v with
  | V1  | V2  | V3  | V4 | V5 -> true
  | V6  | V7  | V8 -> false

let v6_or_higher v =
  not (v5_or_lower v)

let v4_or_lower v =
  match v with
  | V1  | V2  | V3  | V4 -> true
  | V5  | V6  | V7  | V8 -> false

let v5_or_higher v =
  not (v4_or_lower v)

let v3_or_lower v =
  match v with
  | V1  | V2  | V3 -> true
  | V4  | V5  | V6  | V7  | V8 -> false

let v4_or_higher v =
  not (v3_or_lower v)
  
(* Bytes 6 and 7 are the initial pc for the main routine. In version 6 (only)
this is the (packed!) address of a routine, so the *following* byte is the first
instruction. In all other versions the main routine is indicated just by
its first instruction. *)

let initial_program_counter story =
  let initial_program_counter_offset = Word_address 6 in
  let pc = read_word story initial_program_counter_offset in
  if (version story) = V6 then Instruction (pc * 4 + 1)
  else Instruction pc

let dictionary_base story =
  let dictionary_base_offset = Word_address 8 in
  Dictionary_base (read_word story dictionary_base_offset)
  
let object_table_base story =
  let object_table_base_offset = Word_address 10 in
  Object_base (read_word story object_table_base_offset)
  
let global_variables_table_base story =
  let global_variables_table_base_offset = Word_address 12 in
  Global_table_base (read_word story global_variables_table_base_offset)

let static_memory_base_offset = Word_address 14

let abbreviations_table_base story =
  let abbreviations_table_base_offset = Word_address 24 in
  Abbreviation_table_base (read_word story abbreviations_table_base_offset)
  
let routine_offset story =
  let routine_offset_offset = Word_address 40 in
  8 * (read_word story routine_offset_offset)

let string_offset story =
  let string_offset_offset = Word_address 42 in
  8 * (read_word story string_offset_offset)


let load filename =
  let file = get_file filename in
  let len = String.length file in
  if len < header_size then
    failwith (Printf.sprintf "%s is not a valid story file" filename)
  else
    let high = dereference_string (address_of_high_byte static_memory_base_offset) file in
    let low = dereference_string (address_of_low_byte static_memory_base_offset) file in
    let dynamic_length = high * 256 + low in
    if dynamic_length > len then
      failwith (Printf.sprintf "%s is not a valid story file" filename)
    else 
      let dynamic = String.sub file 0 dynamic_length in
      let static = String.sub file dynamic_length (len - dynamic_length) in
      make dynamic static
      
let decode_routine_packed_address story (Packed_routine packed) =
  match version story with
  | V1
  | V2
  | V3 -> Routine (packed * 2)
  | V4
  | V5 -> Routine (packed * 4)
  | V6
  | V7 -> Routine (packed * 4 + (routine_offset story))
  | V8 -> Routine (packed * 8)

let decode_string_packed_address story (Packed_zstring packed) =
  match version story with
  | V1
  | V2
  | V3 -> Zstring (packed * 2)
  | V4
  | V5 -> Zstring (packed * 4)
  | V6
  | V7 -> Zstring (packed * 4 + (string_offset story))
  | V8 -> Zstring (packed * 8)
