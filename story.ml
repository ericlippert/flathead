open Utility
open Type

(* The Z Machine divides memory into dynamic and static; dynamic is always
before static memory. Static memory may not change. We therefore model
memory as a dynamic block that tracks updates and a static block that
never changes at all. *)

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

let original story =
  let original_bytes = Immutable_bytes.original story.dynamic_memory in
  { story with dynamic_memory = original_bytes }

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

(* Writes bytes into memory; no zstring encoding, no zero
termination, no length. *)
let write_string story str text =
  let length = String.length text in
  let rec aux i story =
    if i = length then
      story
    else
      let story = write_byte story (byte_of_string str i) (int_of_char text.[i]) in
      aux (i + 1) story in
  aux 0 story

(* Writes a series of bytes into memory. Does not zstring encode them.
   Does zero-byte terminate them. *)
let write_string_zero_terminate story sz text =
  let null_byte = 0 in
  let length = String.length text in
  let str = string_of_sz sz in
  let story = write_string story str text in
  let terminator = byte_of_string str length in
  write_byte story terminator null_byte

(* Writes a series of bytes into memory; no zero terminator,
prefixed by two bytes of length *)
let write_length_word_prefixed_string story wps text =
  let str = string_of_wps wps in
  let length_addr = length_addr_of_wps wps in
  let story = write_string story str text in
  let length = String.length text in
  write_word story length_addr length

(* Writes a series of bytes into memory; no zero terminator,
prefixed by one byte of length *)
let write_length_byte_prefixed_string story bps text =
  let str = string_of_bps bps in
  let length_addr = length_addr_of_bps bps in
  let story = write_string story str text in
  let length = String.length text in
  write_byte story length_addr length

(* Debugging method for displaying a raw block of memory. *)
let display_story_bytes story address length =
  let get_byte addr = read_byte story addr in
  display_bytes get_byte address length

(* *)
(* Header *)
(* *)

let header_size = 64

(* Header byte 0 is the version number, from 1 to 8. *)

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

let flags1_offset = Byte_address 1
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

let release_number_offset = Word_address 2

let release_number story =
  Release_number (read_word story release_number_offset)

(* Bytes 4 and 5 of the header indicate the start of "high" memory. The
high memory mark was useful for early interpreters; the idea was that
low memory would be kept in physical memory, and high memory could be
paged into physical memory as needed. This interpreter makes no use
of the high memory mark. *)

let high_memory_base story =
  let high_memory_base_offset = Word_address 4 in
  High_memory_base (read_word story high_memory_base_offset)

(* Bytes 6 and 7 are the initial pc for the main routine. In version 6 (only)
this is the (packed!) address of a routine, so the *following* byte is the first
instruction. In all other versions the main routine is indicated just by
its first instruction. *)

let initial_program_counter story =
  let initial_program_counter_offset = Word_address 6 in
  let pc = read_word story initial_program_counter_offset in
  if (version story) = V6 then Instruction (pc * 4 + 1)
  else Instruction pc

(* Spec: The dictionary table is held in static memory and its byte address
  is stored in the word at $08 in the header. *)

let dictionary_base story =
  let dictionary_base_offset = Word_address 8 in
  Dictionary_base (read_word story dictionary_base_offset)

(* The object table address is stored at byte 10 *)

let object_table_base story =
  let object_table_base_offset = Word_address 10 in
  Object_base (read_word story object_table_base_offset)

let global_variables_table_base story =
  let global_variables_table_base_offset = Word_address 12 in
  Global_table_base (read_word story global_variables_table_base_offset)

let static_memory_base_offset = Word_address 14
let static_memory_base story =
  Static_memory_base (read_word story static_memory_base_offset)

let flags2_offset = Word_address 16

let flags2 story =
  read_word story flags2_offset

let set_flags2 story value =
  write_word story flags2_offset value

let flags2_bit story bit =
  read_word_bit story flags2_offset bit

let set_flags2_bit_to story bit value =
  write_set_word_bit_to story flags2_offset bit value

let transcript_bit = bit0
let transcript_enabled story =
  Transcript_enabled (flags2_bit story transcript_bit)

let set_transcript_enabled story (Transcript_enabled value) =
  set_flags2_bit_to story transcript_bit value

let force_fixed_pitch_bit = bit1
let force_fixed_pitch story =
  Force_fixed_pitch (flags2_bit story force_fixed_pitch_bit)

let draw_status_requested_bit = bit2
let draw_status_requested story =
  Draw_status_requested (flags2_bit story draw_status_requested_bit)

let set_draw_status_requested story (Draw_status_requested value) =
  set_flags2_bit_to story draw_status_requested_bit value

let pictures_requested_bit = bit3
let pictures_requested story =
  Pictures_requested (flags2_bit story pictures_requested_bit)

let set_pictures_requested story (Pictures_requested value) =
  set_flags2_bit_to story pictures_requested_bit value

let undo_requested_bit = bit4
let undo_requested story =
  Undo_requested (flags2_bit story undo_requested_bit)

let set_undo_requested story (Undo_requested value) =
  set_flags2_bit_to story undo_requested_bit value

let mouse_requested_bit = bit5
let mouse_requested story =
  Mouse_requested (flags2_bit story mouse_requested_bit)

let set_mouse_requested story (Mouse_requested value) =
  set_flags2_bit_to story mouse_requested_bit value

let colours_requested story =
  let colours_requested_bit = bit6 in
  Colours_requested (flags2_bit story colours_requested_bit)

let sound_requested_bit = bit7
let sound_requested story =
  Sound_requested (flags2_bit story sound_requested_bit)

let set_sound_requested story (Sound_requested value) =
  set_flags2_bit_to story sound_requested_bit value

let menus_requested_bit = bit8
let menus_requested story =
  Menus_requested (flags2_bit story menus_requested_bit)

let set_menus_requested story (Menus_requested value) =
  set_flags2_bit_to story menus_requested_bit value

let serial_number story =
  let start_offset = 18 in
  let end_offset = 24 in
  let string_of_byte addr =
    let b = read_byte story (Byte_address addr) in
    string_of_char (char_of_int b) in
  Serial_number (accumulate_strings_loop string_of_byte start_offset end_offset)

let abbreviations_table_base story =
  let abbreviations_table_base_offset = Word_address 24 in
  Abbreviation_table_base (read_word story abbreviations_table_base_offset)

(* Spec: The file length stored at $1a is actually divided by a constant,
depending on the Version, to make it fit into a header word. This constant
is 2 for Versions 1 to 3, 4 for Versions 4 to 5 or 8 for Versions 6 and later. *)

let file_size story =
  let file_size_offset = Word_address 26 in
  let s = read_word story file_size_offset in
  let m = match (version story) with
  | V1  | V2  | V3 -> 2
  | V4  | V5 -> 4
  | _ -> 8 in
  File_size (s * m)

let header_checksum story =
  let checksum_offset = Word_address 28 in
  Checksum (read_word story checksum_offset)

(* The checksum is simply the bottom two bytes of the sum of all the
   bytes in the original story file, not counting the header. *)
let compute_checksum story =
  let orig = original story in
  let (File_size size) = file_size story in
  let size = Byte_address size in
  let rec aux acc addr =
    if addr = size then
      acc
    else
      let byte = read_byte orig addr in
      let sum = unsigned_word (acc + byte) in
      aux sum (inc_byte_addr addr) in
  Checksum (aux 0 (Byte_address header_size))

let verify_checksum story =
  let h = header_checksum story in
  let c = compute_checksum story in
  h = c

let interpreter_number_offset = Byte_address 30
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

let interpreter_version_offset = Byte_address 31
let interpreter_version story =
  Interpreter_version (read_byte story interpreter_version_offset)

let set_interpreter_version story (Interpreter_version version) =
  write_byte story interpreter_version_offset version

let screen_height_offset = Byte_address 32
let screen_height story =
  Character_height (read_byte story screen_height_offset)

let set_screen_height story (Character_height height) =
  write_byte story screen_height_offset height

let screen_width_offset = Byte_address 33
let screen_width story =
  Character_width (read_byte story screen_width_offset)

let set_screen_width story (Character_width width) =
  write_byte story screen_width_offset width

let screen_height_units_offset = Word_address 34
let screen_height_units story =
  Pixel_height (read_word story screen_height_units_offset)

let set_screen_height_units story (Pixel_height height) =
  write_word story screen_height_units_offset height

let screen_width_units_offset = Word_address 36
let screen_width_units story =
  Pixel_width (read_word story screen_width_units_offset)

let set_screen_width_units story (Pixel_width width) =
  write_word story screen_width_units_offset width

(* The font height and width header bytes are swapped in version 6. *)

let font_height_offset story =
  if (version story) = V6 then (Byte_address 38) else (Byte_address 39)

let font_height story =
  Pixel_height (read_byte story (font_height_offset story))

let set_font_height story (Pixel_height height) =
  write_byte story (font_height_offset story) height

let font_width_offset story =
  if (version story) = V6 then (Byte_address 39) else (Byte_address 38)

let font_width story =
  Pixel_width (read_byte story (font_width_offset story))

let set_font_width story (Pixel_width width) =
  write_byte story (font_width_offset story) width

let routine_offset story =
  let routine_offset_offset = Word_address 40 in
  8 * (read_word story routine_offset_offset)

let string_offset story =
  let string_offset_offset = Word_address 42 in
  8 * (read_word story string_offset_offset)

let default_background_colour_offset = Byte_address 44
let default_background_colour story =
  Colour (read_byte story default_background_colour_offset)

let set_default_background_colour story (Colour colour) =
  write_byte story default_background_colour_offset colour

let default_foreground_colour_offset = Byte_address 45
let default_foreground_colour story =
  Colour (read_byte story default_foreground_colour_offset)

let set_default_foreground_colour story (Colour colour) =
  write_byte story default_foreground_colour_offset colour

let terminating_characters_offset = Word_address 46
let terminating_characters_base story =
  Terminating_characters_base (read_word story terminating_characters_offset)

let text_width_offset = Word_address 48
let text_width story =
  Pixel_width (read_word story text_width_offset)

let set_text_width story (Pixel_width width) =
  write_word story text_width_offset width

let standard_major_offset = Byte_address 50
let standard_minor_offset = Byte_address 51
let standard_revision story =
  Revision (
    (read_byte story standard_major_offset),
    (read_byte story standard_minor_offset))

let set_standard_revision story (Revision (major, minor)) =
  let story = write_byte story standard_major_offset major in
  write_byte story standard_minor_offset minor

let alphabet_table story =
  let alphabet_table_offset = Word_address 52 in
  Alphabet_table (read_word story alphabet_table_offset)

let header_extension story =
  let header_extension_offset = Word_address 54 in
  Word_address (read_word story header_extension_offset)

let header_extension_word story offset =
  let base = header_extension story in
  if base = Word_address 0 then 0
  else read_word story (inc_word_addr_by base offset)

let set_header_extension_word story offset value =
  let base = header_extension story in
  if base = Word_address 0 then story
  else write_word story (inc_word_addr_by base offset) value

let mouse_x_offset = 1
let mouse_x story =
  Pixel_x (header_extension_word story mouse_x_offset)

let set_mouse_x story (Pixel_x x)=
  set_header_extension_word story x mouse_x_offset

let mouse_y_offset = 2
let mouse_y story =
  Pixel_y (header_extension_word story mouse_y_offset)

let set_mouse_y story (Pixel_y y)=
  set_header_extension_word story y mouse_y_offset

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
