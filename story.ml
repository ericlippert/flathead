(* This contains all the logic for dealing with the Z Machine
story file itself. All state in the story file is in memory;
these functions just provide structure around that memory. *)

open Utility
open Instruction

type t =
{
  memory : Memory.t
}

type object_number =
  Object of int

let invalid_object = Object 0

type property_number =
  Property of int

let invalid_property = Property 0

type dictionary_number =
  Dictionary of int

type attribute_number =
  Attribute of int

type routine_address =
  Routine of int

type packed_routine_address =
  Packed_routine of int

type packed_zstring_address =
  Packed_zstring of int


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
  | 3 -> Zstring.Address (packed * 2)
  | 4
  | 5 -> Zstring.Address (packed * 4)
  | 6
  | 7 -> Zstring.Address (packed * 4 + (string_offset story))
  | 8 -> Zstring.Address (packed * 8)
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

let abbreviation_address story (Zstring.Abbreviation n) =
  if n < 0 || n >= abbreviation_table_length then
    failwith "bad offset into abbreviation table"
  else
    let abbr_addr = (abbreviations_table_base story) + (n * 2) in
    let word_addr = read_word story abbr_addr in
    Zstring.Address (decode_word_address word_addr)

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
    let address = abbreviation_address story (Zstring.Abbreviation i) in
    let value = read_zstring story address in
    let (Zstring.Address address) = address in
    Printf.sprintf "%02x: %04x  %s\n" i address value in
  accumulate_strings_loop to_string 0 abbreviation_table_length

(* *)
(* Object table *)
(* *)

let default_property_table_size story =
  if (version story) <= 3 then 31 else 63

let default_property_table_entry_size = 2

let default_property_table_base = object_table_base

let default_property_value story (Property n) =
  if n < 1 || n > (default_property_table_size story) then
    failwith "invalid index into default property table"
  else
    let base = default_property_table_base story in
    let addr = (base + (n - 1) * default_property_table_entry_size) in
    read_word story addr

(* A debugging method for looking at the default property table *)
let display_default_property_table story =
  let to_string i =
    let value = default_property_value story (Property i) in
    Printf.sprintf "%02x: %04x\n" i value in
  accumulate_strings_loop to_string 1 ((default_property_table_size story) + 1)

let object_tree_base story =
  let prop_base = default_property_table_base story in
  let table_size = default_property_table_size story in
  prop_base + default_property_table_entry_size * table_size

let object_table_entry_size story =
  if (version story) <= 3 then 9 else 14

let object_address story (Object object_number) =
  let tree_base = object_tree_base story in
  let entry_size = object_table_entry_size story in
  tree_base + (object_number - 1) * entry_size

let object_attributes_word_1 story object_number =
  read_word story (object_address story object_number)

let object_attributes_word_2 story object_number =
  let attributes2_offset = 2 in
  let obj_addr = object_address story object_number in
  read_word story (obj_addr + attributes2_offset)

let object_attributes_word_3 story object_number =
  if (version story) <= 3 then
    0
  else
    let attributes3_offset = 3 in
    let obj_addr = object_address story object_number in
    read_word story (obj_addr + attributes3_offset)

let attribute_count story =
  if (version story) <= 3 then 32 else 48

let object_attribute_address story object_number (Attribute attribute_number) =
  if attribute_number < 0 || attribute_number >= (attribute_count story) then
    failwith "bad attribute"
  else
    let offset = attribute_number / 8 in
    let address = (object_address story object_number) + offset in
    let bit = Bit_number (7 - (attribute_number mod 8)) in
    (address, bit)

let object_attribute story object_number attribute_number =
  let (address, bit) = object_attribute_address story object_number attribute_number in
  let byte = read_byte story address in
  fetch_bit bit byte

let set_object_attribute story object_number attribute_number =
  let (address, bit) = object_attribute_address story object_number attribute_number in
  let byte = read_byte story address in
  write_byte story address (set_bit bit byte)

let clear_object_attribute story object_number attribute_number =
  let (address, bit) = object_attribute_address story object_number attribute_number in
  let byte = read_byte story address in
  write_byte story address (clear_bit bit byte)

let object_parent story object_number =
  let obj_addr = object_address story object_number in
  if (version story) <= 3 then
    Object (read_byte story (obj_addr + 4))
  else
    Object (read_word story (obj_addr + 6))

let set_object_parent story object_number (Object new_parent) =
  let obj_addr = object_address story object_number in
  if (version story) <= 3 then
    write_byte story (obj_addr + 4) new_parent
  else
    write_word story (obj_addr + 6) new_parent

let object_sibling story object_number =
  let obj_addr = object_address story object_number in
  if (version story) <= 3 then
    Object (read_byte story (obj_addr + 5))
  else
    Object (read_word story (obj_addr + 8))

let set_object_sibling story object_number (Object new_sibling) =
  let obj_addr = object_address story object_number in
  if (version story) <= 3 then
    write_byte story (obj_addr + 5) new_sibling
  else
    write_word story (obj_addr + 8) new_sibling

let object_child_offset story =
  if (version story) <= 3 then 6 else 10

let object_child story object_number =
  let obj_addr = object_address story object_number in
  if (version story) <= 3 then
    Object (read_byte story (obj_addr + 6))
  else
    Object (read_word story (obj_addr + 10))

let set_object_child story object_number (Object new_child) =
  let obj_addr = object_address story object_number in
  if (version story) <= 3 then
    write_byte story (obj_addr + 6) new_child
  else
    write_word story (obj_addr + 10) new_child

(* The last two bytes in an object description are a pointer to a
block that contains additional properties. *)
let object_property_address story object_number =
  let object_property_offset = if (version story) <= 3 then 7 else 12 in
  let obj_addr = object_address story object_number in
  read_word story (obj_addr + object_property_offset)

(* Oddly enough, the Z machine does not ever say how big the object table is.
   Assume that the address of the first property block in the first object is
   the bottom of the object tree table. *)
let object_count story =
  let table_start = object_tree_base story in
  let table_end = object_property_address story (Object 1) in
  let entry_size = object_table_entry_size story in
  (table_end - table_start) / entry_size

(* The property entry begins with a length-prefixed zstring *)
let object_name story n =
  let addr = object_property_address story n in
  let length = read_byte story addr in
  if length = 0 then "<unnamed>"
  else read_zstring story (Zstring.Address (addr + 1))

let find_previous_sibling story child =
  let rec aux current =
    let next_sibling = object_sibling story current in
    if next_sibling = child then current
    else aux next_sibling in
  let parent = object_parent story child in
  let first_child = object_child story parent in
  aux first_child

(* Takes a child object and detatches it from its parent *)

let remove_object story child =
  let original_parent = object_parent story child in
  if original_parent = invalid_object then
    story (* Already detatched *)
  else
    (* First edit: if the child is the parent's first child then
      make the next sibling the new first child.  If the child
      is not the first child then the previous sibling
      needs to point to the next sibling. *)
    let edit1 = (
      let sibling = object_sibling story child in
      if child = object_child story original_parent then
        set_object_child story original_parent sibling
      else
        let prev_sibling = find_previous_sibling story child in
        set_object_sibling story prev_sibling sibling) in
    (* Second edit: the child now has no parent. *)
    set_object_parent edit1 child invalid_object

    (* Takes a child object and a parent object, and causes the child to be the
    first child of the parent. *)

let insert_object story child parent =
  (* Detatch the new child from its old parent *)
  let edit1 = remove_object story child in
  (* Hook up the new child to its new parent *)
  let edit2 = set_object_parent edit1 child parent in
  (* Hook up the sibling chain *)
  let edit3 = set_object_sibling edit2 child (object_child edit2 parent) in
  (* Make the child the new first child of the parent *)
  set_object_child edit3 parent child

(* Not every object has every property. An object's properties are a
   zero-terminated block of memory where the first byte indicates the
   property number and the number of bytes in the property value.
   This block begins after the length-prefixed object name. *)

(* Takes the address of a property data block -- past the string.
Returns the length of the header, the length of the data, and the
property number. *)

let decode_property_data story address =
  let b = read_byte story address in
  if b = 0 then
    (0, 0, invalid_property)
  else if (version story) <= 3 then
    (* In version 3 it's easy. The number of bytes of property data
    is indicated by the top 3 bits; the property number is indicated
    by the bottom 5 bits, and the header is one byte. *)
    (1, (fetch_bits bit7 size3 b) + 1, Property (fetch_bits bit4 size5 b))
  else
    (* In version 4 the property number is the bottom 6 bits. *)
    let property_number = Property (fetch_bits bit5 size6 b) in
    (* If the high bit of the first byte is set then the length is
      indicated by the bottom six bits of the *following* byte.
      The following byte needs to have its high bit set as well.
      (See below).

      If the high bit is not set then the length is indicated by
      the sixth bit. *)
    if fetch_bit bit7 b then
      let len = fetch_bits bit5 size6 (read_byte story (address + 1)) in
      (2, (if len = 0 then 64 else len), property_number)
    else
      (1, (if fetch_bit bit6 b then 2 else 1), property_number)

(* This method produces a list of (number, data_length, data_address) tuples *)
let property_addresses story object_number =
  let rec aux acc address =
    let b = read_byte story address in
    if b = 0 then
      acc
    else
      let (header_length, data_length, property_number) =
        decode_property_data story address in
      let this_property =
        (property_number, data_length, address + header_length) in
      let next_addr = address + header_length + data_length in
      aux (this_property :: acc) next_addr in
  let property_name_address = object_property_address story object_number in
  let property_name_word_length = read_byte story property_name_address in
  let first_property_address = property_name_address + 1 + property_name_word_length * 2 in
  aux [] first_property_address

(* Given the adddress of the data block, how long is it?  In version 3
this is easy; we just look at the previous byte. In version 4 there could
be two bytes before the data, but the one immediately before the data is
always the size byte. If its high bit is on then the bottom six bits are the
size. If the high bit is not on then the size is determined by bit 6. *)
let property_length_from_address story address =
  if address = 0 then
    0
  else
    let b = read_byte story (address - 1) in
    if (version story) <= 3 then
      1 + (fetch_bits bit7 size3 b)
    else
      if fetch_bit bit7 b then
        let len = fetch_bits bit5 size6 b in
        if len = 0 then 64 else len
      else
        if fetch_bit bit6 b then 2 else 1

(* Given an object and property number, what is the address
   of the associated property block? Or zero if there is none. *)
let property_address story object_number property_number =
  let rec aux addresses =
    match addresses with
    | [] -> 0
    | (number, _, address) :: tail ->
      if number = property_number then address
      else aux tail in
  aux (property_addresses story object_number)

(* Fetch the one or two byte value associated with a given property of a given object.
If the object does not have that property then fetch the default property value. *)
let object_property story object_number property_number =
  (* We simply do a linear search for the property, even though they are
     stored in sorted order. The blocks we are searching are first, variable
     size, which makes them inconvenient to binary search. And second, are
     small, making binary search not worth the bother. *)
  let rec aux addresses =
    match addresses with
    | [] -> default_property_value story property_number
    | (number, length, address) :: tail ->
      if number = property_number then (
        if length = 1 then
          read_byte story address
        else if length = 2 then
          read_word story address
        else
          let (Object n) = object_number in
          let (Property p) = property_number in
          failwith (Printf.sprintf "object %d property %d length %d bad property length" n p length))
      else
        aux tail in
  aux (property_addresses story object_number)

(* Given a property number, find the first property of an object
greater than it. Note that this assumes that properties are enumerated in
order by property_addresses. Returns zero if there is no such property. *)
let get_next_property story object_number (Property property_number) =
  let rec aux addrs =
    match addrs with
    | [] -> invalid_property
    | (Property number, _, _) :: tail ->
      if number > property_number then Property number
      else aux tail in
  aux (property_addresses story object_number)

(* Writes a one or two byte property associated with a given object. *)
(* The property must exist and must be one or two bytes. *)
let write_property story object_number property_number value =
  let rec aux addresses =
    match addresses with
    | [] -> (0, 0)
    | (number, length, address) :: tail ->
      if number = property_number then (address, length)
      else aux tail in
  let (address, length) = aux (property_addresses story object_number) in
  if address = 0 then failwith "invalid property";
  match length with
  | 1 -> write_byte story address value
  | 2 -> write_word story address value
  | _ -> failwith "property cannot be set";;

(* Debugging method for displaying the property numbers and
   values for a given object *)
let display_properties story object_number =
  let to_string (property_number, length, address) =
    let (Property p) = property_number in
    let prop_number_text = Printf.sprintf "%02x" p in
    let prop_value_text =
      if length = 1 || length = 2 then
        let prop_value = object_property story object_number property_number in
        Printf.sprintf ":%04x " prop_value
      else
        " " in
    prop_number_text ^ prop_value_text in
  let addresses = property_addresses story object_number in
  accumulate_strings to_string addresses

let display_object_table story =
  let count = object_count story in
  let to_string i =
    let current = Object i in
    let flags1 = object_attributes_word_1 story current in
    let flags2 = object_attributes_word_2 story current in
    let flags3 = object_attributes_word_3 story current in
    let (Object parent) = object_parent story current in
    let (Object sibling) = object_sibling story current in
    let (Object child) = object_child story current in
    let properties = object_property_address story current in
    let name = object_name story current in
    let object_text =
      Printf.sprintf "%04d: %04x%04x%04x %04x %04x %04x %04x %s "
      i flags1 flags2 flags3 parent sibling child properties name in
    let properties_text = display_properties story current in
    object_text ^ properties_text ^ "\n" in
  accumulate_strings_loop to_string 1 (count + 1)

(* Count down all the objects in the object table and record which ones have no parent. *)
let object_roots story =
  let rec aux object_number acc =
    let current = Object object_number in
    if current = invalid_object then
      acc
    else if (object_parent story current) = invalid_object then
      aux (object_number - 1) (current :: acc)
    else
      aux (object_number - 1) acc in
  aux (object_count story) []

let display_object_tree story =
  let rec aux acc indent object_number =
    if object_number = invalid_object then
      acc
    else
      let name = object_name story object_number in
      let child = object_child story object_number in
      let sibling = object_sibling story object_number in
      let (Object n) = object_number in
      let object_text =
        Printf.sprintf "%s%04d %s\n" indent n name in
      let with_object = acc ^ object_text in
      let new_indent = "    " ^ indent in
      let with_children = aux with_object new_indent child in
      aux with_children indent sibling in
  let to_string object_number =
    aux "" "" object_number in
  accumulate_strings to_string (object_roots story)

(* *)
(* Dictionary *)
(* *)

(* The table is laid out as follows. First there is a header:

byte giving the number of word separators
the word separators, one byte each
byte giving the number of bytes in each dictionary entry
word giving the number of table entries which follow

Each entry is either 4 (in V1-3) or 6 (otherwise) bytes of zstring data,
followed by enough bytes to make up the size of the dictionary entry. *)

let word_separators_count story =
  read_byte story (dictionary_base story)

let word_separators story =
  let base = dictionary_base story in
  let count = read_byte story base in
  let rec aux acc i =
    if i < 1 then acc
    else aux ((read_byte story (base + i)) :: acc) (i - 1) in
  aux [] count

let dictionary_entry_length story =
  let base = dictionary_base story in
  let separators = word_separators_count story in
  read_byte story (base + separators + 1)

let dictionary_max_word_length story =
  if (version story) <= 3 then 6 else 9

let dictionary_entry_count story =
  let base = dictionary_base story in
  let separators = word_separators_count story in
  read_word story (base + separators + 2)

let dictionary_table_base story =
  let base = dictionary_base story in
  let separators = word_separators_count story in
  base + separators + 4

let dictionary_entry_address story (Dictionary dictionary_number) =
  let table_base = dictionary_table_base story in
  let entry_length = dictionary_entry_length story in
  table_base + dictionary_number * entry_length

let dictionary_entry story dictionary_number =
  let addr = dictionary_entry_address story dictionary_number in
  read_zstring story (Zstring.Address addr)

(* Takes a string and finds the address of the corresponding zstring
  in the dictionary *)
(* Note this computes the address of the dictionary string, not the dictionary
  entry number. *)

(* This returns zero if the string cannot be found. Of course zero is a valid
address in the Z-machine; it's the location of the version number. But it
is conventionally used here as an invalid address. *)
let dictionary_lookup story text =
  let count = dictionary_entry_count story in
  let max = dictionary_max_word_length story in
  let truncated = truncate text max in
  let compare i = String.compare (dictionary_entry story (Dictionary i)) truncated in
  match binary_search 0 count compare with
  | None -> 0
  | Some entry_index -> dictionary_entry_address story (Dictionary entry_index)

let display_dictionary story =
  let entry_count = dictionary_entry_count story in
  let header =
    (Printf.sprintf "Separator count: %d\n" (word_separators_count story)) ^
    (Printf.sprintf "Entry length:    %d\n" (dictionary_entry_length story)) ^
    (Printf.sprintf "Entry count:     %d\n" entry_count) in
  let to_string i =
    Printf.sprintf "%04x: %s\n" i (dictionary_entry story (Dictionary i)) in
  header ^ (accumulate_strings_loop to_string 0 entry_count)

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
