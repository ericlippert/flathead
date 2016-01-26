open Story
open Utility
open Type

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

let tree_base story =
  let prop_base = default_property_table_base story in
  let table_size = default_property_table_size story in
  prop_base + default_property_table_entry_size * table_size

let entry_size story =
  if (version story) <= 3 then 9 else 14

let address story (Object object_number) =
  let tree_base = tree_base story in
  let entry_size = entry_size story in
  tree_base + (object_number - 1) * entry_size

let attributes_word_1 story object_number =
  read_word story (address story object_number)

let attributes_word_2 story object_number =
  let attributes2_offset = 2 in
  let obj_addr = address story object_number in
  read_word story (obj_addr + attributes2_offset)

let attributes_word_3 story object_number =
  if (version story) <= 3 then
    0
  else
    let attributes3_offset = 3 in
    let obj_addr = address story object_number in
    read_word story (obj_addr + attributes3_offset)

let attribute_count story =
  if (version story) <= 3 then 32 else 48

let attribute_address story object_number (Attribute attribute_number) =
  if attribute_number < 0 || attribute_number >= (attribute_count story) then
    failwith "bad attribute"
  else
    let offset = attribute_number / 8 in
    let address = (address story object_number) + offset in
    let bit = Bit_number (7 - (attribute_number mod 8)) in
    (address, bit)

let attribute story object_number attribute_number =
  let (address, bit) = attribute_address story object_number attribute_number in
  let byte = read_byte story address in
  fetch_bit bit byte

let set_attribute story object_number attribute_number =
  let (address, bit) = attribute_address story object_number attribute_number in
  let byte = read_byte story address in
  write_byte story address (set_bit bit byte)

let clear_attribute story object_number attribute_number =
  let (address, bit) = attribute_address story object_number attribute_number in
  let byte = read_byte story address in
  write_byte story address (clear_bit bit byte)

let parent story object_number =
  let obj_addr = address story object_number in
  if (version story) <= 3 then
    Object (read_byte story (obj_addr + 4))
  else
    Object (read_word story (obj_addr + 6))

let set_parent story object_number (Object new_parent) =
  let obj_addr = address story object_number in
  if (version story) <= 3 then
    write_byte story (obj_addr + 4) new_parent
  else
    write_word story (obj_addr + 6) new_parent

let sibling story object_number =
  let obj_addr = address story object_number in
  if (version story) <= 3 then
    Object (read_byte story (obj_addr + 5))
  else
    Object (read_word story (obj_addr + 8))

let set_sibling story object_number (Object new_sibling) =
  let obj_addr = address story object_number in
  if (version story) <= 3 then
    write_byte story (obj_addr + 5) new_sibling
  else
    write_word story (obj_addr + 8) new_sibling

let child story object_number =
  let obj_addr = address story object_number in
  if (version story) <= 3 then
    Object (read_byte story (obj_addr + 6))
  else
    Object (read_word story (obj_addr + 10))

let set_child story object_number (Object new_child) =
  let obj_addr = address story object_number in
  if (version story) <= 3 then
    write_byte story (obj_addr + 6) new_child
  else
    write_word story (obj_addr + 10) new_child

(* The last two bytes in an object description are a pointer to a
block that contains additional properties. *)
let property_header_address story object_number =
  let object_property_offset = if (version story) <= 3 then 7 else 12 in
  let obj_addr = address story object_number in
  read_word story (obj_addr + object_property_offset)

(* Oddly enough, the Z machine does not ever say how big the object table is.
   Assume that the address of the first property block in the first object is
   the bottom of the object tree table. *)
let count story =
  let table_start = tree_base story in
  let table_end = property_header_address story (Object 1) in
  let entry_size = entry_size story in
  (table_end - table_start) / entry_size

(* The property entry begins with a length-prefixed zstring *)
let name story n =
  let addr = property_header_address story n in
  let length = read_byte story addr in
  if length = 0 then "<unnamed>"
  else read_zstring story (Zstring (addr + 1))

let find_previous_sibling story obj =
  let rec aux current =
    let next_sibling = sibling story current in
    if next_sibling = obj then current
    else aux next_sibling in
  let parent = parent story obj in
  let first_child = child story parent in
  aux first_child

(* Takes a child object and detatches it from its parent *)

let remove story obj =
  let original_parent = parent story obj in
  if original_parent = invalid_object then
    story (* Already detatched *)
  else
    (* First edit: if the child is the parent's first child then
      make the next sibling the new first child.  If the child
      is not the first child then the previous sibling
      needs to point to the next sibling. *)
    let edit1 = (
      let sibling = sibling story obj in
      if obj = child story original_parent then
        set_child story original_parent sibling
      else
        let prev_sibling = find_previous_sibling story obj in
        set_sibling story prev_sibling sibling) in
    (* Second edit: the child now has no parent. *)
    set_parent edit1 obj invalid_object

(* Takes a child object and a parent object, and causes the child to be the
first child of the parent. *)
let insert story new_child new_parent =
  (* Detatch the new child from its old parent *)
  let edit1 = remove story new_child in
  (* Hook up the new child to its new parent *)
  let edit2 = set_parent edit1 new_child new_parent in
  (* Hook up the sibling chain *)
  let edit3 = set_sibling edit2 new_child (child edit2 new_parent) in
  (* Make the child the new first child of the parent *)
  set_child edit3 new_parent new_child

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
  let property_name_address = property_header_address story object_number in
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
let property story object_number property_number =
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
let next_property story object_number (Property property_number) =
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
        let prop_value = property story object_number property_number in
        Printf.sprintf ":%04x " prop_value
      else
        " " in
    prop_number_text ^ prop_value_text in
  let addresses = property_addresses story object_number in
  accumulate_strings to_string addresses

let display_object_table story =
  let count = count story in
  let to_string i =
    let current = Object i in
    let flags1 = attributes_word_1 story current in
    let flags2 = attributes_word_2 story current in
    let flags3 = attributes_word_3 story current in
    let (Object parent) = parent story current in
    let (Object sibling) = sibling story current in
    let (Object child) = child story current in
    let name = name story current in
    let object_text =
      Printf.sprintf "%04d: %04x%04x%04x %04x %04x %04x %s "
      i flags1 flags2 flags3 parent sibling child name in
    let properties_text = display_properties story current in
    object_text ^ properties_text ^ "\n" in
  accumulate_strings_loop to_string 1 (count + 1)

(* Count down all the objects in the object table and record which ones have no parent. *)
let roots story =
  let rec aux object_number acc =
    let current = Object object_number in
    if current = invalid_object then
      acc
    else if (parent story current) = invalid_object then
      aux (object_number - 1) (current :: acc)
    else
      aux (object_number - 1) acc in
  aux (count story) []

let display_object_tree story =
  let rec aux acc indent object_number =
    if object_number = invalid_object then
      acc
    else
      let name = name story object_number in
      let child = child story object_number in
      let sibling = sibling story object_number in
      let (Object n) = object_number in
      let object_text =
        Printf.sprintf "%s%04d %s\n" indent n name in
      let with_object = acc ^ object_text in
      let new_indent = "    " ^ indent in
      let with_children = aux with_object new_indent child in
      aux with_children indent sibling in
  let to_string object_number =
    aux "" "" object_number in
  accumulate_strings to_string (roots story)
