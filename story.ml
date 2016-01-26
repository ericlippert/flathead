(* This contains all the logic for dealing with the Z Machine
story file itself. All state in the story file is in memory;
these functions just provide structure around that memory. *)

open Utility
open Instruction
open Type

type t =
{
  memory : Memory.t
}

(* TODO: Move these somewhere more appropriate *)
let invalid_object = Object 0
let invalid_property = Property 0

(* *)
(* Dealing with memory *)
(* *)

let original story =
  { memory = Memory.original story.memory }

(* A "word address" is only used in the abbreviation table, and is always
just half the real address. A "packed address" is used in calls and fetching
strings, and is half the real address in v3 but different for other versions. *)

let decode_word_address word_address =
  word_address * 2

let read_word story address =
  Memory.read_word story.memory address

let read_byte story address =
  Memory.read_byte story.memory address

let write_word story address value =
  { memory = Memory.write_word story.memory address value }

let write_byte story address value =
  { memory = Memory.write_byte story.memory address value }

(* Writes bytes into memory; no zstring encoding, no zero
termination, no length. *)
let write_string story address text =
  let length = String.length text in
  let rec aux i s =
    if i = length then s
    else aux (i + 1) (write_byte s (address + i) (int_of_char text.[i])) in
  aux 0 story

(* Writes a series of bytes into memory. Does not zstring encode them.
   Does zero-byte terminate them. *)
let write_string_zero_terminate story address text =
  let length = String.length text in
  let copied = write_string story address text in
  write_byte copied (address + length) 0

(* Writes a series of bytes into memory; no zero terminator,
prefixed by two bytes of length *)
let write_length_word_prefixed_string story address text =
  let copied = write_string story (address + 2) text in
  let length = String.length text in
  write_word copied address length

(* Writes a series of bytes into memory; no zero terminator,
prefixed by one byte of length *)
let write_length_byte_prefixed_string story address text =
  let copied = write_string story (address + 1) text in
  let length = String.length text in
  write_byte copied address length

(* Debugging method for displaying a raw block of memory. *)
let display_story_bytes story address length =
  let get_byte addr = read_byte story addr in
  display_bytes get_byte address length

(* *)
(* Header *)
(* *)

(* TODO: Header features beyond v3 *)

let header_size = 64
let version_offset = 0
let version story =
  read_byte story version_offset;;

let flags1 story =
  let flags1_offset = 1 in
  read_byte story flags1_offset

type status_line_kind_type =
  | NoStatus
  | ScoreStatus
  | TimeStatus

let status_line_kind story =
  match (version story, fetch_bit bit1 (flags1 story))  with
  | (1, _)
  | (2, _)
  | (3, false) -> ScoreStatus
  | (3, true) -> TimeStatus
  | _ -> NoStatus

let supports_multiple_windows story =
  let windows_supported_bit = bit5 in
  fetch_bit windows_supported_bit (flags1 story)

let set_supports_multiple_windows story value =
  let flags1_offset = 1 in
  let windows_supported_bit = bit5 in
  let new_flags1 = (set_bit_to windows_supported_bit (flags1 story) value) in
  write_byte story flags1_offset new_flags1

(* TODO: More Flags 1 *)

let release_number story =
  let release_number_offset = 2 in
  read_word story release_number_offset

let high_memory_base story =
  let high_memory_base_offset = 4 in
  read_word story high_memory_base_offset

let initial_program_counter story =
  let initial_program_counter_offset = 6 in
  Instruction (read_word story initial_program_counter_offset)

let dictionary_base story =
  (* Spec: The dictionary table is held in static memory and its byte address
    is stored in the word at $08 in the header. *)
  let dictionary_base_offset = 8 in
  read_word story dictionary_base_offset

let object_table_base story =
  let object_table_base_offset = 10 in
  read_word story object_table_base_offset

let global_variables_table_base story =
  let global_variables_table_base_offset = 12 in
  read_word story global_variables_table_base_offset

let static_memory_base_offset = 14;;
let static_memory_base story =
  read_word story static_memory_base_offset

let flags2 story =
  let flags2_offset = 16 in
  read_byte story flags2_offset

let get_transcript_flag story =
  let transcript_bit = bit0 in
  fetch_bit transcript_bit (flags2 story)

let set_transcript_flag story value =
  let flags2_offset = 16 in
  let transcript_bit = bit0 in
  let new_flags2 = (set_bit_to transcript_bit (flags2 story) value) in
  write_byte story flags2_offset new_flags2

let get_sound_effects_flag story =
  let sfx_bit = bit7 in
  fetch_bit sfx_bit (flags2 story)

(* TODO: Turn this off when starting / restoring *)
let set_sound_effects_flag story value =
  let flags2_offset = 16 in
  let sfx_bit = bit0 in
  let new_flags2 = (set_bit_to sfx_bit (flags2 story) value) in
  write_byte story flags2_offset new_flags2

let serial_number story =
  let start_offset = 18 in
  let end_offset = 24 in
  let string_of_byte addr =
    let b = read_byte story addr in
  string_of_char (char_of_int b) in
  accumulate_strings_loop string_of_byte start_offset end_offset

let abbreviations_table_base story =
  let abbreviations_table_base_offset = 24 in
  read_word story abbreviations_table_base_offset

let file_size story =
  let file_size_offset = 26 in
  let s = read_word story file_size_offset in
  let m = match (version story) with
  | 1
  | 2
  | 3 -> 2
  | 4
  | 5 -> 4
  | _ -> 8 in
  s * m

let header_checksum story =
  let checksum_offset = 28 in
  read_word story checksum_offset

(* The checksum is simply the bottom two bytes of the sum of all the
   bytes in the original story file, not counting the header. *)
let compute_checksum story =
  let orig = original story in
  let size = file_size story in
  let rec aux acc addr =
    if addr >= size then acc
    else
      let byte = read_byte orig addr in
      aux (unsigned_word (acc + byte)) (addr + 1) in
  aux 0 header_size

let verify_checksum story =
  let h = header_checksum story in
  let c = compute_checksum story in
  h = c

let screen_height story =
  let screen_height_offset = 32 in
  read_byte story screen_height_offset

let set_screen_height story height =
  let screen_height_offset = 32 in
  write_byte story screen_height_offset height

let screen_width story =
  let screen_width_offset = 33 in
  read_byte story screen_width_offset

let set_screen_width story width =
  let screen_width_offset = 33 in
  write_byte story screen_width_offset width

let routine_offset story =
  let routine_offset_offset = 40 in
  8 * (read_word story routine_offset_offset)

let string_offset story =
  let string_offset_offset = 42 in
  8 * (read_word story string_offset_offset)

let display_header story =
  let (Instruction ipc) = initial_program_counter story in
  Printf.sprintf "Version                     : %d\n" (version story) ^
  Printf.sprintf "Release number              : %d\n" (release_number story) ^
  Printf.sprintf "Serial number               : %s\n" (serial_number story) ^
  Printf.sprintf "Checksum                    : %04x\n" (header_checksum story) ^
  Printf.sprintf "File size                   : %d\n" (file_size story) ^
  Printf.sprintf "Abbreviations table base    : %04x\n" (abbreviations_table_base story) ^
  Printf.sprintf "Object table base           : %04x\n" (object_table_base story) ^
  Printf.sprintf "Global variables table base : %04x\n" (global_variables_table_base story) ^
  Printf.sprintf "Static memory base          : %04x\n" (static_memory_base story) ^
  Printf.sprintf "Dictionary base             : %04x\n" (dictionary_base story) ^
  Printf.sprintf "High memory base            : %04x\n" (high_memory_base story) ^
  Printf.sprintf "Initial program counter     : %04x\n" ipc

let decode_routine_packed_address story (Packed_routine packed) =
  match version story with
  | 1
  | 2
  | 3 -> Routine (packed * 2)
  | 4
  | 5 -> Routine (packed * 4)
  | 6
  | 7 -> Routine (packed * 4 + (routine_offset story))
  | 8 -> Routine (packed * 8)
  | _ -> failwith "bad version"

let decode_string_packed_address story (Packed_zstring packed) =
  match version story with
  | 1
  | 2
  | 3 -> Zstring (packed * 2)
  | 4
  | 5 -> Zstring (packed * 4)
  | 6
  | 7 -> Zstring (packed * 4 + (string_offset story))
  | 8 -> Zstring (packed * 8)
  | _ -> failwith "bad version"

let load_story filename =
  let file = get_file filename in
  let len = String.length file in
  if len < header_size then
    failwith (Printf.sprintf "%s is not a valid story file" filename);
  let high = int_of_char file.[static_memory_base_offset] in
  let low = int_of_char file.[static_memory_base_offset + 1] in
  let dynamic_length = high * 256 + low in
  if (dynamic_length > len) then
    failwith (Printf.sprintf "%s is not a valid story file" filename);
  let dynamic = String.sub file 0 dynamic_length in
  let static = String.sub file dynamic_length (len - dynamic_length) in
  { memory = Memory.make dynamic static };;

(* *)
(* Abbreviation table and string decoding *)
(* *)

let abbreviation_table_length = 96

let abbreviation_address story (Abbreviation n) =
  if n < 0 || n >= abbreviation_table_length then
    failwith "bad offset into abbreviation table"
  else
    let abbr_addr = (abbreviations_table_base story) + (n * 2) in
    let word_addr = read_word story abbr_addr in
    Zstring (decode_word_address word_addr)

(* gives the length in bytes of the encoded zstring, not the decoded string *)
let zstring_length story address =
  Zstring.length (read_word story) address

let rec read_zstring story address =
  let read_abbrv a =
    let abbr_addr = abbreviation_address story a in
    read_zstring story abbr_addr in
  Zstring.read (read_word story) read_abbrv address

(* Debugging helper *)
let display_zchar_bytes story offset length =
  Zstring.display_bytes (read_word story) offset length

(* Debugging helper *)
let display_abbreviation_table story =
  let to_string i =
    let address = abbreviation_address story (Abbreviation i) in
    let value = read_zstring story address in
    let (Zstring address) = address in
    Printf.sprintf "%02x: %04x  %s\n" i address value in
  accumulate_strings_loop to_string 0 abbreviation_table_length

(* *)
(* Bytecode *)
(* *)

let decode_instruction story address =
  let reader =
    {
      word_reader = read_word story;
      byte_reader = read_byte story;
      zstring_reader = read_zstring story;
      zstring_length = zstring_length story
    } in
  Instruction.decode reader address (version story)

let maximum_local = 15

let locals_count story (Routine routine_address) =
  let count = read_byte story routine_address in
  if count > maximum_local then failwith "routine must have fewer than 16 locals"
  else count

let first_instruction story (Routine routine_address) =
  (* Spec:
  * A routine begins with one byte indicating the number of local
    variables it has (between 0 and 15 inclusive).
  * In Versions 1 to 4, that number of 2-byte words follows, giving initial
    values for these local variables.
  * In Versions 5 and later, the initial values are all zero.
  * Execution of instructions begins from the byte after this header
    information *)
  if (version story) <= 4 then
    let count = locals_count story (Routine routine_address) in
    Instruction (routine_address + 1 + count * 2)
  else
    Instruction (routine_address + 1)

(* Note that here the locals are indexed from 1 to 15, not 0 to 14 *)
let local_default_value story (Routine routine_address) n =
  if n < 1 || n > maximum_local then
    failwith "invalid local"
  else if (version story) <= 4 then
    read_word story (routine_address + 1 + 2 * (n - 1))
  else
    0

(* *)
(* Serialization *)
(* *)

let compress story =
  let original_story = original story in
  let memory_length = static_memory_base story in
  let rec aux acc i c =
    let string_of_byte b =
      string_of_char (char_of_int b) in
    if i = memory_length then
      acc
    else if c = 256 then
      let encoded = "\000" ^ (string_of_byte (c - 1)) in
      aux (acc ^ encoded) i 0
    else
      let original_byte = read_byte original_story i in
      let current_byte = read_byte story i in
      let combined = original_byte lxor current_byte in
      if combined = 0 then
        aux acc (i + 1) (c + 1)
      else if c > 0 then
        let encoded = "\000" ^ (string_of_byte (c - 1)) ^ (string_of_byte combined) in
        aux (acc ^ encoded) (i + 1) 0
      else
        let encoded = string_of_byte combined in
        aux (acc ^ encoded) (i + 1) 0 in
  aux "" 0 0

let apply_uncompressed_changes story uncompressed =
  (* We cannot simply say "make a new dynamic memory chunk out of
  these bytes" because then the *next* time we load a save game,
  that dynamic memory will be the "original" memory, which is wrong.
  We need to maintain the truly original loaded-off-disk memory. *)
  let original_story = original story in
  let length = String.length uncompressed in
  let rec aux index acc =
    if index >= length then
      acc
    else
      let new_byte = int_of_char uncompressed.[index] in
      let orig_byte = read_byte original_story index in
      let new_story =
        if new_byte = orig_byte then acc
        else write_byte acc index new_byte in
      aux (index + 1) new_story in
  aux 0 original_story

let apply_compressed_changes story compressed =
  let original_story = original story in
  let length = String.length compressed in
  let rec aux index_change index_mem acc =
    if index_change >= length then
      acc
    else
      let b = int_of_char compressed.[index_change] in
      if b = 0 then
        (* TODO: If length - 1 this is a problem *)
        let c = 1 + int_of_char compressed.[index_change + 1] in
        aux (index_change + 2) (index_mem + c) acc
      else
        let orig_byte = read_byte original_story index_mem in
        let new_byte = b lxor orig_byte in
        let new_story = write_byte acc index_mem new_byte in
        aux (index_change + 1) (index_mem + 1) new_story in
  aux 0 0 original_story
