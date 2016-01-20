(* This contains all the logic for dealing with the Z Machine
story file itself. All state in the story file is in memory;
these functions just provide structure around that memory. *)
type t =
{
  memory : Memory.t
}

let accumulate_strings to_string items =
  let folder text item =
    text ^ (to_string item) in
  List.fold_left folder "" items

let accumulate_strings_loop to_string start max =
  let rec aux acc i =
    if i >= max then acc
    else aux (acc ^ (to_string i)) (i + 1) in
  aux "" start

let string_of_char x =
  String.make 1 x

let unsigned_word word =
  ((word mod 65536) + 65536) mod 65536

let signed_word word =
  let canonical = unsigned_word word in
  if canonical > 32767 then canonical - 65536 else canonical

(* Helper method that takes an item and a function that produces related items.
   The result is the transitive closure of the relation. *)

(* TODO: This is not very efficient because of the call to List.mem in there.
   TODO: A solution involving an immutable set would be more performant for
   TODO: large closures. *)

let transitive_closure_many items relation =
  let rec merge related set stack =
    match related with
    | [] -> (set, stack)
    | head :: tail ->
      if List.mem head set then merge tail set stack
      else merge tail (head :: set) (head :: stack) in
  let rec aux set stack =
    match stack with
    | [] -> set
    | head :: tail ->
      let (new_set, new_stack) = merge (relation head) set tail in
      aux new_set new_stack in
  aux [] items

let transitive_closure item relation =
  transitive_closure_many [item] relation

let reflexive_closure_many items relation =
  let t = transitive_closure_many items relation in
  List.fold_left (fun s i -> if List.mem i s then s else i :: s) t items

let reflexive_closure item relation =
  reflexive_closure_many [item] relation

(* *)
(* Dealing with memory *)
(* *)

let original story =
  { memory = Memory.original story.memory }

let fetch_bit n word =
  (word land (1 lsl n)) lsr n = 1

let clear_bit n word =
  word land (lnot (1 lsl n))

let set_bit n word =
  word lor (1 lsl n)

let set_bit_to n word value =
  if value then set_bit n word
  else clear_bit n word

let fetch_bits high length word =
  let mask = lnot (-1 lsl length) in
  (word lsr (high - length + 1)) land mask

(* A "word address" is only used in the abbreviation table, and is always
just half the real address. A "packed address" is used in calls and fetching
strings, and is half the real address in v3 but different for other versions. *)

let decode_word_address word_address =
  word_address * 2;;

(* TODO: only works for v3 *)
let decode_packed_address story packed =
  packed * 2

let read_word story address =
  Memory.read_word story.memory address

let read_byte story address =
  Memory.read_byte story.memory address

let write_word story address value =
  { memory = Memory.write_word story.memory address value }

let write_byte story address value =
  { memory = Memory.write_byte story.memory address value }

(* Writes a series of bytes into memory. Does not zstring encode them.
   Does zero-byte terminate them. *)
let write_string story address text =
  let length = String.length text in
  let rec aux i s =
    if i = length then s
    else aux (i + 1) (write_byte s (address + i) (int_of_char text.[i])) in
  let copied = aux 0 story in
  write_byte copied (address + length) 0

(* Debugging method for displaying a raw block of memory. *)
let display_bytes story address length =
  let blocksize = 16 in
  let to_string i =
    let header =
      if i mod blocksize = 0 then Printf.sprintf "\n%06x: " (i + address)
      else "" in
    let byte = read_byte story (i + address) in
    let contents = Printf.sprintf "%02x " byte in
    header ^ contents in
  (accumulate_strings_loop to_string 0 length) ^ "\n"

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
  match (version story, fetch_bit 1 (flags1 story))  with
  | (1, _)
  | (2, _)
  | (3, false) -> ScoreStatus
  | (3, true) -> TimeStatus
  | _ -> NoStatus

(* TODO: More Flags 1 *)

let high_memory_base story =
  let high_memory_base_offset = 4 in
  read_word story high_memory_base_offset

let initial_program_counter story =
  let initial_program_counter_offset = 6 in
  read_word story initial_program_counter_offset

let dictionary_base story =
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
  let transcript_bit = 0 in
  fetch_bit transcript_bit (flags2 story)

let set_transcript_flag story value =
  let flags2_offset = 16 in
  let transcript_bit = 0 in
  let new_flags2 = (set_bit_to transcript_bit (flags2 story) value) in
  write_byte story flags2_offset new_flags2

let abbreviations_table_base story =
  let abbreviations_table_base_offset = 24 in
  read_word story abbreviations_table_base_offset

let file_size story =
  let file_size_offset = 26 in
  (* TODO: Multiplier depends on version *)
  2 * (read_word story file_size_offset)

let header_checksum story =
  let checksum_offset = 28 in
  read_word story checksum_offset

(* The checksum is simply the bottom two bytes of the sum of all the
   bytes in the original story file, not counting the header. *)
let compute_checksum story =
  let original = Memory.original story.memory in
  let size = file_size story in
  let rec aux acc addr =
    if addr >= size then acc
    else
      let byte = Memory.read_byte original addr in
      aux (unsigned_word (acc + byte)) (addr + 1) in
  aux 0 header_size

let verify_checksum story =
  let h = header_checksum story in
  let c = compute_checksum story in
  h = c

let display_header story =
  Printf.sprintf "Version                     : %d\n" (version story) ^
  Printf.sprintf "Abbreviations table base    : %04x\n" (abbreviations_table_base story) ^
  Printf.sprintf "Object table base           : %04x\n" (object_table_base story) ^
  Printf.sprintf "Global variables table base : %04x\n" (global_variables_table_base story) ^
  Printf.sprintf "Static memory base          : %04x\n" (static_memory_base story) ^
  Printf.sprintf "Dictionary base             : %04x\n" (dictionary_base story) ^
  Printf.sprintf "High memory base            : %04x\n" (high_memory_base story) ^
  Printf.sprintf "Initial program counter     : %04x\n" (initial_program_counter story);;

let load_story filename =
  (* TODO: Could read in just the header first, then the dynamic block as a string,
  then the static block as a string. Less copying that way. *)
  let channel = open_in_bin filename in
  let length = in_channel_length channel in
  let file = String.create length in
  really_input channel file 0 length;
  close_in channel;
  let len = String.length file in
  if len < header_size then failwith (Printf.sprintf "%s is not a valid story file" filename);
  let version = int_of_char file.[version_offset] in
  if version <> 3 then failwith (Printf.sprintf "%s is not a valid version 3 story file" filename);
  let high = int_of_char file.[static_memory_base_offset] in
  let low = int_of_char file.[static_memory_base_offset + 1] in
  let dynamic_length = high * 256 + low in
  if (dynamic_length > len) then failwith (Printf.sprintf "%s is not a valid story file" filename);
  let dynamic = String.sub file 0 dynamic_length in
  let static = String.sub file dynamic_length (len - dynamic_length) in
  { memory = Memory.make dynamic static };;

(* *)
(* Abbreviation table and string decoding *)
(* *)

(* TODO: Assumes v3 abbreviation table *)
let abbreviation_table_length = 96;;

let abbreviation_address story n =
  if n < 0 || n >= abbreviation_table_length then
    failwith "bad offset into abbreviation table"
  else
    let abbr_addr = (abbreviations_table_base story) + (n * 2) in
    let word_addr = read_word story abbr_addr in
    decode_word_address word_addr

type string_mode =
  | Alphabet of int
  | Abbreviation of int
  | Leading
  | Trailing of int

let alphabet_table = [|
  " "; "?"; "?"; "?"; "?"; "?"; "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j";
  "k"; "l"; "m"; "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z";
  " "; "?"; "?"; "?"; "?"; "?"; "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J";
  "K"; "L"; "M"; "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W"; "X"; "Y"; "Z";
  " "; "?"; "?"; "?"; "?"; "?"; "?"; "\n"; "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7";
  "8"; "9"; "."; ","; "!"; "?"; "_"; "#"; "'"; "\""; "/"; "\\"; "-"; ":"; "("; ")" |]

(* gives the length in bytes of the encoded zstring, not the decoded string *)
let zstring_length story address =
  let rec aux length current =
    if fetch_bit 15 (read_word story current) then length + 2
    else aux (length + 2) (current + 2) in
  aux 0 address

let rec read_zstring story address =
  (* TODO: Only processes version 3 strings *)

  (* zstrings encode three characters into two-byte words.

  The high bit is the end-of-string marker, followed by three
  five-bit zchars.

  The meaning of the next zchar(s) depends on the current.

  If the current zchar is 1, 2 or 3 then the next is an offset
  into the abbreviation table; fetch the string indicated there.

  If the current zchar is 4 or 5 then the next is an offset into the
  uppercase or punctuation alphabets, except if the current is 5
  and the next is 6. In that case the two zchars following are a single
  10-bit character. *)

  let process_zchar zchar mode =
    match (mode, zchar) with
    | (Alphabet _, 0) -> (" ", mode)
    | (Alphabet _, 1) -> ("", Abbreviation 0)
    | (Alphabet _, 2) -> ("", Abbreviation 32)
    | (Alphabet _, 3) -> ("", Abbreviation 64)
    | (Alphabet _, 4) -> ("", Alphabet 1)
    | (Alphabet _, 5) -> ("", Alphabet 2)
    | (Alphabet 2, 6) -> ("", Leading)
    | (Alphabet a, _) -> (alphabet_table.(a * 32 + zchar), Alphabet 0)
    | (Abbreviation a, _) ->
      let abbr_addr = abbreviation_address story (a + zchar) in
      (read_zstring story abbr_addr, Alphabet 0)
    | (Leading, _) -> ("", (Trailing zchar))
    | (Trailing high, _) ->
      let s = string_of_char (Char.chr (high * 32 + zchar)) in
      (s, Alphabet 0) in

  let rec aux acc mode1 current_address =
    let zchar_bit_size = 5 in
    let word = read_word story current_address in
    let is_end = fetch_bit 15 word in
    let zchar1 = fetch_bits 14 zchar_bit_size word in
    let zchar2 = fetch_bits 9 zchar_bit_size word in
    let zchar3 = fetch_bits 4 zchar_bit_size word in
    let (text1, mode2) = process_zchar zchar1 mode1 in
    let (text2, mode3) = process_zchar zchar2 mode2 in
    let (text3, mode_next) = process_zchar zchar3 mode3 in
    let new_acc = acc ^ text1 ^ text2 ^ text3 in
    if is_end then new_acc
    else aux new_acc mode_next (current_address + 2) in
  aux "" (Alphabet 0) address

  (* A debugging method for looking at memory broken up into the
  1 / 5 / 5 / 5 bit chunks used by zstrings. *)

  let display_zchar_bytes story offset length =
    let rec aux i acc =
      if i > length then acc
      else (
        let word = read_word story (offset + i) in
        let is_end = fetch_bits 15 1 word in
        let zchar1 = fetch_bits 14 5 word in
        let zchar2 = fetch_bits 9 5 word in
        let zchar3 = fetch_bits 4 5 word in
        let s = Printf.sprintf "%04x(%01x %02x %02x %02x) " word is_end zchar1 zchar2 zchar3 in
        aux (i + 2) (acc ^ s)) in
      aux 0 ""

let display_abbreviation_table story =
  let to_string i =
    let address = abbreviation_address story i in
    let value = read_zstring story address in
    Printf.sprintf "%02x: %04x  %s\n" i address value in
  accumulate_strings_loop to_string 0 abbreviation_table_length

(* *)
(* Object table *)
(* *)

(* TODO: 63 in version 4 and above *)
let default_property_table_size story =
  31

let default_property_table_entry_size = 2

let default_property_table_base = object_table_base;;

let default_property_value story n =
  if n < 1 || n > (default_property_table_size story) then
    failwith "invalid index into default property table"
  else
    let base = default_property_table_base story in
    let addr = (base + (n - 1) * default_property_table_entry_size) in
    read_word story addr

(* A debugging method for looking at the default property table *)
let display_default_property_table story =
  let to_string i =
    let value = default_property_value story i in
    Printf.sprintf "%02x: %04x\n" i value in
  accumulate_strings_loop to_string 1 ((default_property_table_size story) + 1)

let object_tree_base story =
  let prop_base = default_property_table_base story in
  let table_size = default_property_table_size story in
  prop_base + default_property_table_entry_size * table_size

(* TODO: Object table entry is larger in version 4 *)
let object_table_entry_size story =
  9

let object_address story object_number =
  let tree_base = object_tree_base story in
  let entry_size = object_table_entry_size story in
  tree_base + (object_number - 1) * entry_size

let object_attributes_word_1 story object_number =
  read_word story (object_address story object_number)

let object_attributes_word_2 story object_number =
  let attributes2_offset = 2 in
  let obj_addr = object_address story object_number in
  read_word story (obj_addr + attributes2_offset)

let attribute_count story =
  32
(* TODO: 48 attributes in version 4 *)

let object_attribute_address story object_number attribute_number =
  if attribute_number < 0 || attribute_number >= (attribute_count story) then
    failwith "bad attribute"
  else
    let offset = attribute_number / 8 in
    let address = (object_address story object_number) + offset in
    let bit = 7 - (attribute_number mod 8) in
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

let object_parent_offset = 4

let object_parent story object_number =
  let obj_addr = object_address story object_number in
  read_byte story (obj_addr + object_parent_offset)

let set_object_parent story object_number new_parent =
  let obj_addr = object_address story object_number in
  write_byte story (obj_addr + object_parent_offset) new_parent

let object_sibling_offset = 5

let object_sibling story object_number =
  let obj_addr = object_address story object_number in
  read_byte story (obj_addr + object_sibling_offset)

let set_object_sibling story object_number new_sibling =
  let obj_addr = object_address story object_number in
  write_byte story (obj_addr + object_sibling_offset) new_sibling

let object_child_offset = 6

let object_child story object_number =
  let obj_addr = object_address story object_number in
  read_byte story (obj_addr + object_child_offset)

let set_object_child story object_number new_child =
  let obj_addr = object_address story object_number in
  write_byte story (obj_addr + object_child_offset) new_child

let object_property_offset = 7

let object_property_address story object_number =
  let obj_addr = object_address story object_number in
  read_word story (obj_addr + object_property_offset)

(* Oddly enough, the Z machine does not ever say how big the object table is.
   Assume that the address of the first property block in the first object is
   the bottom of the object tree table. *)
let object_count story =
  let table_start = object_tree_base story in
  let table_end = object_property_address story 1 in
  let entry_size = object_table_entry_size story in
  (table_end - table_start) / entry_size

let object_name story n =
  let addr = object_property_address story n in
  let length = read_byte story addr in
  if length = 0 then "<unnamed>"
  else read_zstring story (addr + 1)

let find_previous_sibling story child =
  let rec aux current =
    let next_sibling = object_sibling story current in
    if next_sibling = child then current
    else aux next_sibling in
  let parent = object_parent story child in
  let first_child = object_child story parent in
  aux first_child

let invalid_object = 0

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
   property number and the number of bytes in the property value. *)

(* This method produces a list of (number, length, address) tuples *)
let property_addresses story object_number =
  let rec aux acc address =
    let b = read_byte story address in
    if b = 0 then
      acc
    else
      let property_length = (fetch_bits 7 3 b) + 1 in
      let property_number = (fetch_bits 4 5 b) in
      let this_property = (property_number, property_length, address + 1) in
      let next_addr = address + 1 + property_length in
      aux (this_property :: acc) next_addr in
  let property_header_address = object_property_address story object_number in
  let property_name_word_length = read_byte story property_header_address in
  let first_property_address = property_header_address + 1 + property_name_word_length * 2 in
  aux [] first_property_address

(* Takes the address of a property data block, returns the length.
The length is always in the top three bits of the byte before the block. *)

let property_length_from_address story address =
  if address = 0 then 0
  else 1 + (fetch_bits 7 3 (read_byte story (address - 1)))

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
          failwith (Printf.sprintf "object %d property %d length %d bad property length" object_number property_number length))
      else
        aux tail in
  aux (property_addresses story object_number)

(* Given a property number, find the first property of an object
greater than it. Note that this assumes that properties are enumerated in
order by property_addresses. Returns zero if there is no such property. *)
let get_next_property story object_number property_number =
  let rec aux addrs =
    match addrs with
    | [] -> 0
    | (number, _, _) :: tail ->
      if number > property_number then number
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
    let prop_number_text = Printf.sprintf "%02x" property_number in
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
    let flags1 = object_attributes_word_1 story i in
    let flags2 = object_attributes_word_2 story i in
    let parent = object_parent story i in
    let sibling = object_sibling story i in
    let child = object_child story i in
    let properties = object_property_address story i in
    let name = object_name story i in
    let object_text =
      Printf.sprintf "%02x: %04x%04x %02x %02x %02x %04x %s "
      i flags1 flags2 parent sibling child properties name in
    let properties_text = display_properties story i in
    object_text ^ properties_text ^ "\n" in
  accumulate_strings_loop to_string 1 (count + 1)

(* Count down all the objects in the object table and record which ones have no parent. *)
let object_roots story =
  let rec aux object_number acc =
    if object_number = invalid_object then
      acc
    else if (object_parent story object_number) = invalid_object then
      aux (object_number - 1) (object_number :: acc)
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
      let object_text =
        Printf.sprintf "%s%02x %s\n" indent object_number name in
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

(* TODO: Only supports version 3 *)

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

(* TODO: 9 in v4 and up *)
let dictionary_max_word_length story =
  6

let dictionary_entry_count story =
  let base = dictionary_base story in
  let separators = word_separators_count story in
  read_word story (base + separators + 2)

let dictionary_table_base story =
  let base = dictionary_base story in
  let separators = word_separators_count story in
  base + separators + 4

let dictionary_entry_address story dictionary_number =
  let table_base = dictionary_table_base story in
  let entry_length = dictionary_entry_length story in
  table_base + dictionary_number * entry_length

let dictionary_entry story dictionary_number =
  let addr = dictionary_entry_address story dictionary_number in
  read_zstring story addr

(* Binary search a range. Min is inclusive, max is exclusive. *)
let rec binary_search min max compare =
  if min >= max then
    None
  else
    let middle = (min + max) / 2 in
    let comparison = compare middle in
    if comparison < 0 then binary_search (middle + 1) max compare
    else if comparison > 0 then binary_search min middle compare
    else Some middle

let truncate text length =
  if (String.length text) > length then String.sub text 0 length
  else text

(* Takes a string and finds the address of the corresponding zstring
  in the dictionary *)
(* Note this computes the address of the dictionary string, not the dictionary
  entry number. *)
let dictionary_lookup story text =
  let count = dictionary_entry_count story in
  let max = dictionary_max_word_length story in
  let truncated = truncate text max in
  let compare i = String.compare (dictionary_entry story i) truncated in
  match binary_search 0 count compare with
  | None -> 0
  | Some entry_index -> dictionary_entry_address story entry_index

let display_dictionary story =
  let entry_count = dictionary_entry_count story in
  let header =
    (Printf.sprintf "Separator count: %d\n" (word_separators_count story)) ^
    (Printf.sprintf "Entry length:    %d\n" (dictionary_entry_length story)) ^
    (Printf.sprintf "Entry count:     %d\n" entry_count) in
  let to_string i =
    Printf.sprintf "%04x: %s\n" i (dictionary_entry story i) in
  header ^ (accumulate_strings_loop to_string 0 entry_count)

(* *)
(* Bytecode *)
(* *)

(* TODO: Extended *)
type opcode_form =
  | Long_form
  | Short_form
  | Variable_form

type operand_count =
  | OP0
  | OP1
  | OP2
  | VAR

type variable_location =
  | Stack
  | Local of int
  | Global of int

type operand_type =
  | Large_operand
  | Small_operand
  | Variable_operand
  | Omitted

type operand =
  | Large of int
  | Small of int
  | Variable of variable_location

type bytecode =
            | OP2_1   | OP2_2   | OP2_3   | OP2_4   | OP2_5   | OP2_6   | OP2_7
  | OP2_8   | OP2_9   | OP2_10  | OP2_11  | OP2_12  | OP2_13  | OP2_14  | OP2_15
  | OP2_16  | OP2_17  | OP2_18  | OP2_19  | OP2_20  | OP2_21  | OP2_22  | OP2_23
  | OP2_24  | OP2_25  | OP2_26  | OP2_27  | OP2_28
  | OP1_128 | OP1_129 | OP1_130 | OP1_131 | OP1_132 | OP1_133 | OP1_134 | OP1_135
  | OP1_136 | OP1_137 | OP1_138 | OP1_139 | OP1_140 | OP1_141 | OP1_142 | OP1_143
  | OP0_176 | OP0_177 | OP0_178 | OP0_179 | OP0_180 | OP0_181 | OP0_182 | OP0_183
  | OP0_184 | OP0_185 | OP0_186 | OP0_187 | OP0_188 | OP0_189 | OP0_190 | OP0_191
  | VAR_224 | VAR_225 | VAR_226 | VAR_227 | VAR_228 | VAR_229 | VAR_230 | VAR_231
  | VAR_232 | VAR_233 | VAR_234 | VAR_235 | VAR_236 | VAR_237 | VAR_238 | VAR_239
  | VAR_240 | VAR_241 | VAR_242 | VAR_243 | VAR_244 | VAR_245 | VAR_246 | VAR_247
  | VAR_248 | VAR_249 | VAR_250 | VAR_251 | VAR_252 | VAR_253 | VAR_254 | VAR_255
  | ILLEGAL

(* The tables which follow are maps from the opcode identification number
   to the opcode type; the exact order matters. *)

let one_operand_bytecodes = [|
  OP1_128; OP1_129; OP1_130; OP1_131; OP1_132; OP1_133; OP1_134; OP1_135;
  OP1_136; OP1_137; OP1_138; OP1_139; OP1_140; OP1_141; OP1_142; OP1_143  |]

let zero_operand_bytecodes = [|
  OP0_176; OP0_177; OP0_178; OP0_179; OP0_180; OP0_181; OP0_182; OP0_183;
  OP0_184; OP0_185; OP0_186; OP0_187; OP0_188; OP0_189; OP0_190; OP0_191  |]

let two_operand_bytecodes =[|
  ILLEGAL; OP2_1;  OP2_2;  OP2_3;  OP2_4;  OP2_5;   OP2_6;   OP2_7;
  OP2_8;   OP2_9;  OP2_10; OP2_11; OP2_12; OP2_13;  OP2_14;  OP2_15;
  OP2_16;  OP2_17; OP2_18; OP2_19; OP2_20; OP2_21;  OP2_22;  OP2_23;
  OP2_24;  OP2_25; OP2_26; OP2_27; OP2_28; ILLEGAL; ILLEGAL; ILLEGAL |]

let var_operand_bytecodes = [|
  VAR_224; VAR_225; VAR_226; VAR_227; VAR_228; VAR_229; VAR_230; VAR_231;
  VAR_232; VAR_233; VAR_234; VAR_235; VAR_236; VAR_237; VAR_238; VAR_239;
  VAR_240; VAR_241; VAR_242; VAR_243; VAR_244; VAR_245; VAR_246; VAR_247;
  VAR_248; VAR_249; VAR_250; VAR_251; VAR_252; VAR_253; VAR_254; VAR_255 |]

type branch_address =
  | Return_true
  | Return_false
  | Branch_address of int

type instruction =
{
  opcode : bytecode;
  address : int;
  length : int;
  operands : operand list;
  store : variable_location option;
  branch : (bool * branch_address) option;
  text : string option;
}

let is_call opcode =
  match opcode with
  | VAR_224 (* call / call_vs *)
  | OP1_143 (* call_1n *)
  | OP1_136 (* call_1s *)
  | OP2_26  (* call_2n *)
  | OP2_25  (* call_2s *)
  | VAR_249 (* call_vn *)
  | VAR_250 (* call_vn2 *)
  | VAR_236 (* call_vs2 *) -> true
  | _ -> false

let decode_variable n =
  if n = 0 then Stack
  else if n < 0x10 then Local n
  else Global n

(* Takes the address of an instruction and produces the instruction *)
let decode_instruction story address =
  (* Helper methods for decoding *)
  let has_branch opcode =
    match opcode with
    | OP2_1   | OP2_2   | OP2_3   | OP2_4   | OP2_5   | OP2_6   | OP2_7   | OP2_10
    | OP1_128 | OP1_129 | OP1_130 | OP0_181 | OP0_182 | OP0_189 | OP0_191
    | VAR_247 | VAR_255 -> true
    | _ -> false in

  let has_store opcode =
    match opcode with
    | OP2_8   | OP2_9   | OP2_15  | OP2_16  | OP2_17  | OP2_18  | OP2_19
    | OP2_20  | OP2_21  | OP2_22  | OP2_23  | OP2_24  | OP2_25
    | OP1_129 | OP1_130 | OP1_131 | OP1_132 | OP1_136 | OP1_142 | OP1_143
    | VAR_224 | VAR_231 | VAR_236 | VAR_246 | VAR_247 | VAR_248 -> true
    | _ -> false in

  (* These opcodes store to a variable identified as a "small" rather
  than as a "variable". *)

  let is_special_store opcode =
    match opcode with
    | OP2_4   (* dec_chk *)
    | OP2_5   (* inc_chk *)
    | OP2_13  (* store *)
    | OP1_133 (* inc *)
    | OP1_134 (* dec *)
    | OP1_142 (* load *)
    | VAR_233 (* pull *) -> true
    | _ -> false in

  let has_text opcode =
    match opcode with
    | OP0_178 | OP0_179 -> true
    | _ -> false in

  let decode_types n =
    match n with
    | 0 -> Large_operand
    | 1 -> Small_operand
    | 2 -> Variable_operand
    | _ -> Omitted in

  let decode_variable_types types =
    let rec aux i acc =
      if i > 3 then
        acc
      else
        match decode_types (fetch_bits (i * 2 + 1) 2 types) with
        | Omitted -> aux (i + 1) acc
        | x -> aux (i + 1) (x :: acc) in
      aux 0 [] in



  let rec decode_operands operand_address operand_types =
    match operand_types with
    | [] -> []
    | Large_operand :: types ->
      let head = Large (read_word story operand_address) in
      let tail = decode_operands (operand_address + 2) types in
      head :: tail
    | Small_operand :: types ->
      let head = Small (read_byte story operand_address) in
      let tail = decode_operands (operand_address + 1) types in
      head :: tail
    | Variable_operand :: types ->
      let head = Variable (decode_variable (read_byte story operand_address)) in
      let tail = decode_operands (operand_address + 1) types in
      head :: tail
    | Omitted :: _ ->
      failwith "omitted operand type passed to decode operands" in

    let rec operand_size operand_types =
      match operand_types with
      | [] -> 0
      | Large_operand :: types -> 2 + (operand_size types)
      | _ :: types -> 1 + (operand_size types) in

    (* Spec 4.7 *)
    let branch_size branch_code_address =
      let b = read_byte story branch_code_address in
      if (fetch_bit 6 b) then 1 else 2 in

    let decode_branch branch_code_address total_length =
      let high = read_byte story branch_code_address in
      let sense = fetch_bit 7 high in
      let bottom6 = fetch_bits 5 6 high in
      let offset =
        if fetch_bit 6 high then
          bottom6
        else
          let low = read_byte story (branch_code_address + 1) in
          let unsigned = 256 * bottom6 + low in
          if unsigned < 8192 then unsigned else unsigned - 16384 in
        match offset with
        | 0 -> (sense, Return_false)
        | 1 -> (sense, Return_true)
        | _ -> (sense, Branch_address (address + total_length + offset - 2)) in

    let munge_operands instr =
      let munge_store_operands () =
        (* The first operand must be a variable,
        but it is sometimes a value instead *)
        match instr.operands with
        | (Small small) :: tail -> (Variable (decode_variable small)) :: tail
        | _ -> instr.operands in
      let munge_call_operands () =
        (* For calls with constant callees, unpack the address here. *)
        match instr.operands with
        | (Large large) :: tail ->
          (Large (decode_packed_address story large)) :: tail
        | _ -> instr.operands in
        let munge_jump_operands () =
          (* Turn relative jumps into jumps to a specific address. *)
          match instr.operands with
          | [(Large large)] ->
            [(Large (instr.address + instr.length + (signed_word large) - 2))]
          | _ -> instr.operands in
        if is_special_store instr.opcode then munge_store_operands()
        else if is_call instr.opcode then munge_call_operands()
        else if instr.opcode = OP1_140 then munge_jump_operands()
        else instr.operands in

    (* Helper methods are done. Start decoding *)

    (*  SPEC

    4.3 Form and operand count

    Each instruction has a form (long, short, extended or variable) and an
    operand count (0OP, 1OP, 2OP or VAR). If the top two bits of the opcode
    are $$11 the form is variable; if $$10, the form is short. If the opcode
    is 190 ($BE in hexadecimal) and the version is 5 or later, the form is
    "extended". Otherwise, the form is "long".

    4.3.1

    In short form, bits 4 and 5 of the opcode byte give an operand type as
    above. If this is $11 then the operand count is 0OP; otherwise, 1OP. In
    either case the opcode number is given in the bottom 4 bits.

    4.3.2

    In long form the operand count is always 2OP. The opcode number is
    given in the bottom 5 bits.

    4.3.3

    In variable form, if bit 5 is 0 then the count is 2OP; if it is 1,
    then the count is VAR. The opcode number is given in the bottom 5 bits.

    4.3.4 (TODO)

    In extended form, the operand count is VAR. The opcode number is
    given in a second opcode byte.

    *)

    let b = read_byte story address in

    let form = match fetch_bits 7 2 b with
    | 3 -> Variable_form
    | 2 -> Short_form
    | _ -> Long_form in

    let op_count = match form with
    | Variable_form -> if fetch_bit 5 b then VAR else OP2
    | Long_form -> OP2
    | Short_form -> if fetch_bits 5 2 b = 3 then OP0 else OP1 in

    let opcode = match op_count with
    | OP0 -> zero_operand_bytecodes.(fetch_bits 3 4 b)
    | OP1 -> one_operand_bytecodes.(fetch_bits 3 4 b)
    | OP2 -> two_operand_bytecodes.(fetch_bits 4 5 b)
    | VAR -> var_operand_bytecodes.(fetch_bits 4 5 b) in

    let opcode_length = 1 in

    (* SPEC

    4.4 Specifying operand types

    Next, the types of the operands are specified.

    4.4.1

    In short form, bits 4 and 5 of the opcode give the type.

    4.4.2

    In long form, bit 6 of the opcode gives the type of the first operand,
    bit 5 of the second. A value of 0 means a small constant and 1 means a
    variable. (If a 2OP instruction needs a large constant as operand, then
    it should be assembled in variable rather than long form.)

    4.4.3

    In variable or extended forms, a byte of 4 operand types is given next.
    This contains 4 2-bit fields: bits 6 and 7 are the first field, bits 0 and
    1 the fourth. The values are operand types as above. Once one type has
    been given as 'omitted', all subsequent ones must be. Example:
    $$00101111 means large constant followed by variable (and no third or
    fourth opcode).

    *)

    let operand_types = match form with
    | Short_form ->
      (match op_count with
      | OP0 -> []
      | _ -> [decode_types (fetch_bits 5 2 b)])
    | Long_form ->
      (match fetch_bits 6 2 b with
      | 0 -> [ Small_operand; Small_operand ]
      | 1 -> [ Small_operand; Variable_operand ]
      | 2 -> [ Variable_operand; Small_operand ]
      | _ -> [ Variable_operand; Variable_operand ])
    | Variable_form ->
      let type_byte = read_byte story (address + opcode_length) in
      decode_variable_types type_byte in

    let type_length = if form = Variable_form then 1 else 0 in
    let operand_address = address + opcode_length + type_length in
    let operands = decode_operands operand_address operand_types in
    let operand_length = operand_size operand_types in
    let store_address = operand_address + operand_length in
    let store =
      if has_store opcode then
        let store_byte = read_byte story store_address in
        Some (decode_variable store_byte)
      else
        None in
    let store_length = if has_store opcode then 1 else 0 in
    let branch_code_address = store_address + store_length in
    let branch_length =
      if has_branch opcode then branch_size branch_code_address
      else 0 in
    let text_address = branch_code_address + branch_length in
    let text =
      if has_text opcode then
        Some (read_zstring story text_address)
      else
        None in
    let text_length =
      if has_text opcode then
        zstring_length story text_address
      else
        0 in
    let length =
      opcode_length + type_length + operand_length + store_length +
      branch_length + text_length in
    let branch =
      if has_branch opcode then
        Some (decode_branch branch_code_address length)
      else
        None in
    let instr = { opcode; address; length; operands; store; branch; text } in
    { instr with operands = munge_operands instr }
    (* End of decode_instruction *)

let display_instruction instr =
  (* We match Inform's convention of numbering the locals and globals from zero *)
  let display_variable variable =
    match variable with
    | Stack -> "sp"
    | Local local -> Printf.sprintf "local%d" (local - 1)
    | Global global -> Printf.sprintf "g%02x" (global - 16) in

  let display_operands () =
    let to_string operand =
      match operand with
      | Large large -> Printf.sprintf "%04x " large
      | Small small -> Printf.sprintf "%02x " small
      | Variable variable -> (display_variable variable) ^ " " in
    accumulate_strings to_string instr.operands in

  let display_store () =
    match instr.store with
    | None -> ""
    | Some variable -> "->" ^ (display_variable variable) in

  let display_branch () =
    match instr.branch with
    | None -> ""
    | Some (true, Return_false) -> "?false"
    | Some (false, Return_false) -> "?~false"
    | Some (true, Return_true) -> "?true"
    | Some (false, Return_true) -> "?~true"
    | Some (true, Branch_address address) -> Printf.sprintf "?%04x" address
    | Some (false, Branch_address address) -> Printf.sprintf "?~%04x" address in

  let display_text () =
    match instr.text with
    | None -> ""
    | Some str -> str in

  let opcode_name opcode =
    match opcode with
    | ILLEGAL -> "ILLEGAL"
    | OP2_1   -> "je"
    | OP2_2   -> "jl"
    | OP2_3   -> "jg"
    | OP2_4   -> "dec_chk"
    | OP2_5   -> "inc_chk"
    | OP2_6   -> "jin"
    | OP2_7   -> "test"
    | OP2_8   -> "or"
    | OP2_9   -> "and"
    | OP2_10  -> "test_attr"
    | OP2_11  -> "set_attr"
    | OP2_12  -> "clear_attr"
    | OP2_13  -> "store"
    | OP2_14  -> "insert_obj"
    | OP2_15  -> "loadw"
    | OP2_16  -> "loadb"
    | OP2_17  -> "get_prop"
    | OP2_18  -> "get_prop_addr"
    | OP2_19  -> "get_next_prop"
    | OP2_20  -> "add"
    | OP2_21  -> "sub"
    | OP2_22  -> "mul"
    | OP2_23  -> "div"
    | OP2_24  -> "mod"
    | OP2_25  -> "call_2s"
    | OP2_26  -> "call_2n"
    | OP2_27  -> "set_colour"
    | OP2_28  -> "throw"
    | OP1_128 -> "jz"
    | OP1_129 -> "get_sibling"
    | OP1_130 -> "get_child"
    | OP1_131 -> "get_parent"
    | OP1_132 -> "get_prop_len"
    | OP1_133 -> "inc"
    | OP1_134 -> "dec"
    | OP1_135 -> "print_addr"
    | OP1_136 -> "call_1s"
    | OP1_137 -> "remove_obj"
    | OP1_138 -> "print_obj"
    | OP1_139 -> "ret"
    | OP1_140 -> "jump"
    | OP1_141 -> "print_paddr"
    | OP1_142 -> "load"
    | OP1_143 -> "not"
    | OP0_176 -> "rtrue"
    | OP0_177 -> "rfalse"
    | OP0_178 -> "print"
    | OP0_179 -> "print_ret"
    | OP0_180 -> "nop"
    | OP0_181 -> "save"
    | OP0_182 -> "restore"
    | OP0_183 -> "restart"
    | OP0_184 -> "ret_popped"
    | OP0_185 -> "pop"
    | OP0_186 -> "quit"
    | OP0_187 -> "new_line"
    | OP0_188 -> "show_status"
    | OP0_189 -> "verify"
    | OP0_190 -> "EXTENDED TODO"
    | OP0_191 -> "piracy"
    | VAR_224 -> "call"
    | VAR_225 -> "storew"
    | VAR_226 -> "storeb"
    | VAR_227 -> "put_prop"
    | VAR_228 -> "sread"
    | VAR_229 -> "print_char"
    | VAR_230 -> "print_num"
    | VAR_231 -> "random"
    | VAR_232 -> "push"
    | VAR_233 -> "pull"
    | VAR_234 -> "split_window"
    | VAR_235 -> "set_window"
    | VAR_236 -> "call_vs2"
    | VAR_237 -> "erase_window"
    | VAR_238 -> "erase_line"
    | VAR_239 -> "set_cursor"
    | VAR_240 -> "get_cursor"
    | VAR_241 -> "set_text_style"
    | VAR_242 -> "buffer_mode"
    | VAR_243 -> "output_stream"
    | VAR_244 -> "input_stream"
    | VAR_245 -> "sound_effect"
    | VAR_246 -> "read_char"
    | VAR_247 -> "scan_table"
    | VAR_248 -> "not"
    | VAR_249 -> "call_vn"
    | VAR_250 -> "call_vn2"
    | VAR_251 -> "tokenise"
    | VAR_252 -> "encode_text"
    | VAR_253 -> "copy_table"
    | VAR_254 -> "print_table"
    | VAR_255 -> "check_arg_count" in

  let start_addr = instr.address in
  let name = opcode_name instr.opcode in
  let operands = display_operands () in
  let store = display_store() in
  let branch = display_branch() in
  let text = display_text() in
  Printf.sprintf "%04x: %s %s%s %s %s\n"
    start_addr name operands store branch text
  (* End of display_instruction *)

let display_instructions story address count =
  let rec aux acc addr c =
    if c = 0 then
      acc
    else
      let instr = decode_instruction story addr in
      let s = display_instruction instr in
      aux (acc  ^ s) (addr + instr.length) (c - 1) in
  aux "" address count

let continues_to_following opcode =
  match opcode with
  | OP2_28 (* throw *)
  | OP1_139 (* ret *)
  | OP1_140 (* jump *)
  | OP0_176 (* rtrue *)
  | OP0_177 (* rfalse *)
  | OP0_179 (* print_ret *)
  | OP0_183 (* restart *)
  | OP0_184 (* ret_popped *)
  | OP0_186 (* quit *) -> false
  | _ -> true

(* Suppose an instruction either has a branch portion to an address,
or a jump to an address. What is that address? *)
let branch_target instr =
  let br_target =
    match instr.branch with
    | None -> None
    | Some (_, Return_false) -> None
    | Some (_, Return_true) -> None
    | Some (_, Branch_address address) -> Some address in
  let jump_target =
    match (instr.opcode, instr.operands) with
    | (OP1_140, [Large address]) -> Some address
    | _ -> None in
  match (br_target, jump_target) with
  | (Some b, _) -> Some b
  | (_, Some j) -> Some j
  | _ -> None

(* Any given instruction in a routine either goes on to the next instruction,
when it is done, or branches to another instruction when it is done, or terminates
the routine. Given the address of an instruction, what are all the reachable instructions
in this routine? Note that this could miss instructions if a jump is made to a location
read from a variable. *)
let all_reachable_addresses_in_routine story instr_address =
  let immediately_reachable_addresses address =
    let instr = decode_instruction story address in
    let next =
      if continues_to_following instr.opcode then
        [instr.address + instr.length]
      else
        [] in
    match (branch_target instr) with
    | Some address -> address :: next
    | _ -> next in
  reflexive_closure instr_address immediately_reachable_addresses

let display_reachable_instructions story address =
  let reachable = all_reachable_addresses_in_routine story address in
  let sorted = List.sort compare reachable in
  let to_string addr =
    let instr = decode_instruction story addr in
    display_instruction instr in
  accumulate_strings to_string sorted

let locals_count story routine_address =
  let count = read_byte story routine_address in
  if count > 15 then failwith "routine must have fewer than 16 locals"
  else count

let first_instruction story routine_address =
  let count = locals_count story routine_address in
  routine_address + 1 + count * 2

(* Note that here the locals are indexed from 1 to 15, not 0 to 14 *)
let local_default_value story routine_address n =
  if n < 1 || n > 15 then failwith "invalid local"
  else read_word story (routine_address + 1 + 2 * (n - 1));;

let display_routine story routine_address =
  let first = first_instruction story routine_address in
  display_reachable_instructions story first

let call_address instr =
  if is_call instr.opcode then
    match instr.operands with
    | (Large address) :: _ -> Some address
    | _ -> None
  else
    None

(* Takes the address of the first instruction in a routine, produces
   a list of addresses of all routines called in the routine. *)
(* Again, this can miss routines that are called with a variable as the address. *)
let reachable_routines_in_routine story instr_address =
  let reachable_instrs = all_reachable_addresses_in_routine story instr_address in
  let option_fold routines instr_addr =
    match call_address (decode_instruction story instr_addr) with
    | None -> routines
    | Some routine_address -> routine_address :: routines in
  List.fold_left option_fold [] reachable_instrs

let all_routines story =
  let ipc = initial_program_counter story in
  let called_by_main = reachable_routines_in_routine story ipc in
  let relation routine =
      reachable_routines_in_routine story (first_instruction story routine) in
  let all_routines = reflexive_closure_many called_by_main relation in
  List.sort compare all_routines

let display_all_routines story =
  let routines = all_routines story in
  let to_string r =
    (display_routine story r) ^ "\n\n" in
  accumulate_strings to_string routines

let first_global = 16
let current_object_global = 16
let current_score_global = 17
let turn_count_global = 18
let current_hours_global = 17
let current_minute_global = 18
let last_global = 255

(* Note that globals are indexed starting at 16 *)
let read_global story global_number =
  if global_number < first_global || global_number > last_global then
    failwith "global variable index out of range"
  else
    let base = global_variables_table_base story in
    let offset = (global_number - first_global) * 2 in
    read_word story (base + offset)

let display_globals story =
  let to_string g =
    Printf.sprintf "%02x %04x\n" (g - 16) (read_global story g) in
  accumulate_strings_loop to_string 16 256

let write_global story global_number value =
  if global_number < first_global || global_number > last_global then
      failwith "global variable index out of range"
  else
    let base = global_variables_table_base story in
    let offset = (global_number - first_global) * 2 in
    write_word story (base + offset) value

let current_object_name story =
  let current_object = read_global story current_object_global in
  if current_object = invalid_object then ""
  else object_name story current_object

let status_globals story =
  let score = signed_word (read_global story current_score_global) in
  let turn = read_global story turn_count_global in
  (score, turn)
