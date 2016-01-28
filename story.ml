(* This contains all the logic for dealing with the Z Machine
story file itself. All state in the story file is in memory;
these functions just provide structure around that memory. *)

open Utility
open Type

type t =
{
  memory : Memory.t
}

(* *)
(* Dealing with memory *)
(* *)

let original story =
  { memory = Memory.original story.memory }

let read_word story address =
  Memory.read_word story.memory address

let read_byte story address =
  Memory.read_byte story.memory address

let read_bit story address bit =
  fetch_bit bit (read_byte story address)

let write_word story address value =
  { memory = Memory.write_word story.memory address value }

let write_byte story address value =
  { memory = Memory.write_byte story.memory address value }

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

let header_size = 64

(* Header byte 0 is the version number, from 1 to 8. *)

let version_offset = 0
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

(* We often need to know what version we're in, but typically the only
interesting questions are "are we in v4 or better?" and "are we in v5
or better?" *)

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

let display_version v =
  match v with
  | V1 -> "1"
  | V2 -> "2"
  | V3 -> "3"
  | V4 -> "4"
  | V5 -> "5"
  | V6 -> "6"
  | V7 -> "7"
  | V8 -> "8"

(* Header byte 1 is flags. *)

let flags1_offset = 1
let flags1 story =
  read_byte story flags1_offset

let set_flags1 story value =
  write_byte story flags1_offset value

let flags1_bit story bit =
  read_bit story flags1_offset bit

let set_flags1_bit_to story bit value =
  write_set_bit_to story flags1_offset bit value

(* Bit 0 of flags1 indicates whether an interpreter has colours available. *)
(* It is valid only in version 5 and up. *)
(* It should be set by the interpreter on a start / restart / restore. *)

let colours_supported_bit = bit0

let colours_supported story =
  if v4_or_lower (version story) then Colours_supported false
  else Colours_supported (flags1_bit story colours_supported_bit)

let set_colours_supported story (Colours_supported value) =
  if v4_or_lower (version story) then story
  else set_flags1_bit_to story colours_supported_bit value

(* Bit 1 of flags1 indicates whether a version 3 story wants a "score" or a "time"
status line. This is valid only in version 3; version 1 and 2 stories were
always "score", and version 4 and above stories draw their own status lines
in game logic rather than asking the interpreter to do it. *)

(* This bit is not writable by the interpreter in v1 / v2 / v3.  *)

(* In version 6 and above this flag indicates whether the interpreter can
draw pictures. It should be written on restart / restore. *)

let status_line_kind_bit = bit1

let status_line_kind story =
  match (version story, flags1_bit story status_line_kind_bit)  with
  | (V1, _) | (V2, _) | (V3, false) -> ScoreStatus
  | (V3, true) -> TimeStatus
  | _ -> NoStatus

let pictures_supported_bit = bit1

let pictures_supported story =
  if v5_or_lower (version story) then Pictures_supported false
  else Pictures_supported (flags1_bit story pictures_supported_bit)

let set_pictures_supported story (Pictures_supported value) =
  if v5_or_lower (version story) then story
  else set_flags1_bit_to story pictures_supported_bit value

(* Bit 2 of flags1 in v1 / v2 / v3 indicates whether a story file is split across two
disks. We ignore it. In v4 and above it indicates whether boldface is available.
It should be written on start / restart / restore *)

let boldface_supported_bit = bit2

let boldface_supported story =
  if v3_or_lower (version story) then Boldface_supported false
  else Boldface_supported (flags1_bit story boldface_supported_bit)

let set_boldface_supported story (Boldface_supported value) =
  if v3_or_lower (version story) then story
  else set_flags1_bit_to story boldface_supported_bit value

(* Bit 3 of flags1 is the "Tandy" bit in v1/v2/v3.  see Spec Appendix B for
details.  This bit may be set by the interpreter but need not be restored. *)

(* In v4 and above it indicates whether italics are supported; this bit should
be set on start / restart / restore *)

let tandy_bit = bit3

let tandy_mode story =
  if v4_or_higher (version story) then Tandy_mode_enabled false
  else Tandy_mode_enabled (flags1_bit story tandy_bit)

let set_tandy_mode story (Tandy_mode_enabled value) =
  if v4_or_higher (version story) then story
  else set_flags1_bit_to story tandy_bit value

let italics_supported_bit = bit3

let italics_supported story =
  if v3_or_lower (version story) then Italics_supported false
  else Italics_supported (flags1_bit story italics_supported_bit)

let set_italics_supported story (Italics_supported value) =
  if v3_or_lower (version story) then story
  else set_flags1_bit_to story italics_supported_bit value

(* Bit 4 of flags1 indicates whether a v1/v2/v3 interpreter is capable of
displaying a status line. Note that this is inverted in the story memory;
the bit is ON if a status line is NOT available. *)

(* This bit should be set by the interpreter after a start / restart / restore. *)

(* In v4 and above this indicates whether a fixed pitch font is available. *)

(* This bit should be set by the interpreter after a start / restart / restore. *)

let status_line_supported_bit = bit4

let status_line_supported story =
  if v4_or_higher (version story) then Status_line_supported false
  else Status_line_supported (not (flags1_bit story status_line_supported_bit))

let set_status_line_supported story (Status_line_supported value) =
  if v4_or_higher (version story) then story
  else set_flags1_bit_to story italics_supported_bit (not value)

let fixed_pitch_supported_bit = bit4

let fixed_pitch_supported story =
  if v3_or_lower (version story) then Fixed_pitch_supported true
  else Fixed_pitch_supported (flags1_bit story fixed_pitch_supported_bit)

let set_fixed_pitch_supported story (Fixed_pitch_supported value) =
  if v3_or_lower (version story) then story
  else set_flags1_bit_to story fixed_pitch_supported_bit value

(* Bit 5 of flags1 in v1/v2/v3 indicates whether an interpreter is capable of "splitting"
the screen into multiple logical windows. (We presume that all interpreters are
so capable in later versions.) *)

(* This bit should be set by the interpreter after a start / restart / restore. *)

(* In v6 and above this bit indicates whether sound effects are avaialable. *)

(* This bit should be set by the interpreter after a start / restart / restore. *)

let screen_split_supported_bit = bit5

let screen_split_supported story =
  if v4_or_higher (version story) then Screen_split_supported true
  else Screen_split_supported (flags1_bit story screen_split_supported_bit)

let set_screen_split_supported story (Screen_split_supported value) =
  if v4_or_higher (version story) then story
  else set_flags1_bit_to story screen_split_supported_bit value

let sound_effects_supported_bit = bit5

let sound_effects_supported story =
  if v5_or_lower (version story) then Sound_effects_supported false
  else Sound_effects_supported (flags1_bit story sound_effects_supported_bit)

let set_sound_effects_supported story (Sound_effects_supported value) =
  if v5_or_lower (version story) then story
  else set_flags1_bit_to story sound_effects_supported_bit value

(* Bit 6 of flags1 indicates whether an interpreter uses a variable-pitched
typeface by default. *)

(* This bit should be set by the interpreter after a start / restart / restore. *)

let default_is_variable_pitch_bit = bit6

let default_is_variable_pitch story =
  Default_is_variable_pitch (flags1_bit story default_is_variable_pitch_bit)

let set_default_is_variable_pitch story (Default_is_variable_pitch value) =
  set_flags1_bit_to story default_is_variable_pitch_bit value

(* Bit 7 of flags1 indicates whether an interpreter supports timed keyboard input. *)

(* This bit should be set by the interpreter after a start / restart / restore. *)

let timed_keyboard_supported_bit = bit7

let timed_keyboard_supported story =
  Timed_keyboard_supported (flags1_bit story timed_keyboard_supported_bit)

let set_timed_keyboard_supported story (Timed_keyboard_supported value) =
  set_flags1_bit_to story timed_keyboard_supported_bit value

(* Bytes 2 and 3 of the header are by convention the release number. *)

let release_number_offset = 2

let release_number story =
  Release_number (read_word story release_number_offset)

(* Bytes 4 and 5 of the header indicate the start of "high" memory. The
high memory mark was useful for early interpreters; the idea was that
low memory would be kept in physical memory, and high memory could be
paged into physical memory as needed. This interpreter makes no use
of the high memory mark. *)

let high_memory_base story =
  let high_memory_base_offset = 4 in
  High_memory_base (read_word story high_memory_base_offset)

(* Bytes 6 and 7 are the initial pc for the main routine. In version 6 (only)
this is the (packed!) address of a routine, so the *following* byte is the first
instruction. In all other versions the main routine is indicated just by
its first instruction. *)

let initial_program_counter story =
  let initial_program_counter_offset = 6 in
  let pc = read_word story initial_program_counter_offset in
  if (version story) = V6 then Instruction (pc * 4 + 1)
  else Instruction pc

(* Spec: The dictionary table is held in static memory and its byte address
  is stored in the word at $08 in the header. *)

let dictionary_base story =
  let dictionary_base_offset = 8 in
  Dictionary_base (read_word story dictionary_base_offset)

(* The object table address is stored at byte 10 *)

let object_table_base story =
  let object_table_base_offset = 10 in
  Object_base (read_word story object_table_base_offset)

let global_variables_table_base story =
  let global_variables_table_base_offset = 12 in
  Global_table_base (read_word story global_variables_table_base_offset)

let static_memory_base_offset = 14;;
let static_memory_base story =
  Static_memory_base (read_word story static_memory_base_offset)

let flags2_offset = 16

let flags2 story =
  read_byte story flags2_offset

let set_flags2 story value =
  write_byte story flags2_offset value

let flags2_bit story bit =
  read_bit story flags2_offset bit

let set_flags2_bit_to story bit value =
  write_set_bit_to story flags2_offset bit value


(*TODO: Rest of flags 2 flags *)

let transcript_bit = bit0

let get_transcript_flag story =
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
  Serial_number (accumulate_strings_loop string_of_byte start_offset end_offset)

let abbreviations_table_base story =
  let abbreviations_table_base_offset = 24 in
  Abbreviation_table_base (read_word story abbreviations_table_base_offset)

(* Spec: The file length stored at $1a is actually divided by a constant,
depending on the Version, to make it fit into a header word. This constant
is 2 for Versions 1 to 3, 4 for Versions 4 to 5 or 8 for Versions 6 and later. *)

let file_size story =
  let file_size_offset = 26 in
  let s = read_word story file_size_offset in
  let m = match (version story) with
  | V1  | V2  | V3 -> 2
  | V4  | V5 -> 4
  | _ -> 8 in
  File_size (s * m)

let header_checksum story =
  let checksum_offset = 28 in
  Checksum (read_word story checksum_offset)

(* The checksum is simply the bottom two bytes of the sum of all the
   bytes in the original story file, not counting the header. *)
let compute_checksum story =
  let orig = original story in
  let (File_size size) = file_size story in
  let rec aux acc addr =
    if addr >= size then acc
    else
      let byte = read_byte orig addr in
      aux (unsigned_word (acc + byte)) (addr + 1) in
  Checksum (aux 0 header_size)

let verify_checksum story =
  let h = header_checksum story in
  let c = compute_checksum story in
  h = c

let interpreter_number_offset = 30
let interpreter_number story =
  Interpreter_number (read_byte story interpreter_number_offset)

let set_interpreter_number story (Interpreter_number number) =
  write_byte story interpreter_number_offset number

let dec_system_20 = Interpreter_number 1
let apple_iie = Interpreter_number 2
let macintosh = Interpreter_number 3
let amiga = Interpreter_number 4
let atari_st = Interpreter_number 5
let ibm_pc = Interpreter_number 6
let commodore_128 = Interpreter_number 7
let commodore_64 = Interpreter_number 8
let apple_iic = Interpreter_number 9
let apple_iigs = Interpreter_number 10
let tandy_color = Interpreter_number 11

(* TODO: Interpreter version *)

let screen_height story =
  let screen_height_offset = 32 in
  Character_height (read_byte story screen_height_offset)

let set_screen_height story (Character_height height) =
  let screen_height_offset = 32 in
  write_byte story screen_height_offset height

let screen_width story =
  let screen_width_offset = 33 in
  Character_width (read_byte story screen_width_offset)

let set_screen_width story (Character_width width) =
  let screen_width_offset = 33 in
  write_byte story screen_width_offset width

let routine_offset story =
  let routine_offset_offset = 40 in
  8 * (read_word story routine_offset_offset)

let string_offset story =
  let string_offset_offset = 42 in
  8 * (read_word story string_offset_offset)

let display_header story =
  let (Release_number release_number) = release_number story in
  let (Serial_number serial_number) = serial_number story in
  let (Checksum checksum) = header_checksum story in
  let (File_size file_size) = file_size story in
  let (Abbreviation_table_base abbrev_table_base) = abbreviations_table_base story in
  let (Object_base object_table_base) = object_table_base story in
  let (Global_table_base global_table_base) = global_variables_table_base story in
  let (Static_memory_base static_memory_base) = static_memory_base story in
  let (Dictionary_base dictionary_base) = dictionary_base story in
  let (High_memory_base high_memory_base) = high_memory_base story in
  let (Instruction ipc) = initial_program_counter story in
  Printf.sprintf "Version                     : %s\n" (display_version (version story)) ^
  Printf.sprintf "Release number              : %d\n" release_number ^
  Printf.sprintf "Serial number               : %s\n" serial_number ^
  Printf.sprintf "Checksum                    : %04x\n" checksum ^
  Printf.sprintf "File size                   : %d\n" file_size ^
  Printf.sprintf "Abbreviations table base    : %04x\n" abbrev_table_base ^
  Printf.sprintf "Object table base           : %04x\n" object_table_base ^
  Printf.sprintf "Global variables table base : %04x\n" global_table_base ^
  Printf.sprintf "Static memory base          : %04x\n" static_memory_base  ^
  Printf.sprintf "Dictionary base             : %04x\n" dictionary_base ^
  Printf.sprintf "High memory base            : %04x\n" high_memory_base ^
  Printf.sprintf "Initial program counter     : %04x\n" ipc

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
(* Bytecode *)
(* *)

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
  if v4_or_lower (version story) then
    let count = locals_count story (Routine routine_address) in
    Instruction (routine_address + 1 + count * 2)
  else
    Instruction (routine_address + 1)

(* Note that here the locals are indexed from 1 to 15, not 0 to 14 *)
let local_default_value story (Routine routine_address) n =
  if n < 1 || n > maximum_local then
    failwith "invalid local"
  else if v4_or_lower (version story) then
    read_word story (routine_address + 1 + 2 * (n - 1))
  else
    0

(* *)
(* Serialization *)
(* *)

let compress story =
  let original_story = original story in
  let (Static_memory_base memory_length) = static_memory_base story in
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
