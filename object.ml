open Utility
open Type

let invalid_data = Property_data 0
let invalid_object = Object 0
let invalid_property = Property 0

(* The object table is laid out as follows:

* The base of the object table is in the header.
* The object table begins with a block of 31 or 63 default property values.
* Following the default property values is the object tree.
* Each entry in the tree is of the same size, and is laid out as follows:
  * 32 or 48 bits of attribute flags
  * the parent, sibling and child object numbers
  * the address of an additional table of variable-sized properties.
* object numbers are one-based, so zero is used as the invalid object.
*)

let default_property_table_size story =
  if Story.v3_or_lower (Story.version story) then 31 else 63

let default_property_table_entry_size = 2

let default_property_table_base story =
  let (Object_base base)= Story.object_table_base story in
  Property_defaults_table base

let default_property_value story (Property n) =
  if n < 1 || n > (default_property_table_size story) then
    failwith "invalid index into default property table"
  else
    let (Property_defaults_table base) = default_property_table_base story in
    let addr = Word_address ((base + (n - 1) * default_property_table_entry_size)) in
    Story.read_word story addr

(* A debugging method for looking at the default property table *)
let display_default_property_table story =
  let to_string i =
    let value = default_property_value story (Property i) in
    Printf.sprintf "%02x: %04x\n" i value in
  accumulate_strings_loop to_string 1 ((default_property_table_size story) + 1)

let tree_base story =
  let (Object_base base) = Story.object_table_base story in
  let table_size = default_property_table_size story in
  Object_tree_base (base + default_property_table_entry_size * table_size)

let entry_size story =
  if Story.v3_or_lower (Story.version story) then 9 else 14

let address story (Object obj) =
  let (Object_tree_base tree_base) = tree_base story in
  let entry_size = entry_size story in
  Object_address (tree_base + (obj - 1) * entry_size)

let attributes_word_1 story obj =
  let (Object_address addr) = address story obj in
  Story.read_word story (Word_address addr)

let attributes_word_2 story obj =
  let attributes2_offset = 2 in
  let (Object_address addr) = address story obj in
  Story.read_word story (Word_address(addr + attributes2_offset))

let attributes_word_3 story obj =
  if Story.v3_or_lower (Story.version story) then
    0
  else
    let attributes3_offset = 3 in
    let (Object_address addr) = address story obj in
    Story.read_word story (Word_address (addr + attributes3_offset))

let attribute_count story =
  if Story.v3_or_lower (Story.version story) then 32 else 48

let attribute_address story obj (Attribute attribute) =
  if attribute < 0 || attribute >= (attribute_count story) then
    failwith "bad attribute"
  else
    let offset = attribute / 8 in
    let (Object_address obj_addr) = address story obj in
    let bit = Bit_number (7 - (attribute mod 8)) in
    Attribute_address ((Byte_address (obj_addr + offset)), bit)

let attribute story obj attribute =
  let (Attribute_address (address, bit)) = attribute_address story obj attribute in
  Story.read_bit story address bit

let set_attribute story obj attribute =
  let (Attribute_address (address, bit)) = attribute_address story obj attribute in
  Story.write_set_bit story address bit

let clear_attribute story obj attribute =
  let (Attribute_address (address, bit)) = attribute_address story obj attribute in
  Story.write_clear_bit story address bit

let parent story obj =
  let (Object_address addr) = address story obj in
  if Story.v3_or_lower (Story.version story) then
    Object (Story.read_byte story (Byte_address (addr + 4)))
  else
    Object (Story.read_word story (Word_address (addr + 6)))

let set_parent story obj (Object new_parent) =
  let (Object_address addr) = address story obj in
  if Story.v3_or_lower (Story.version story) then
    Story.write_byte story (Byte_address (addr + 4)) new_parent
  else
    Story.write_word story (Word_address (addr + 6)) new_parent

let sibling story obj =
  let (Object_address addr) = address story obj in
  if Story.v3_or_lower (Story.version story) then
    Object (Story.read_byte story (Byte_address (addr + 5)))
  else
    Object (Story.read_word story (Word_address (addr + 8)))

let set_sibling story obj (Object new_sibling) =
  let (Object_address addr) = address story obj in
  if Story.v3_or_lower (Story.version story) then
    Story.write_byte story (Byte_address (addr + 5)) new_sibling
  else
    Story.write_word story (Word_address (addr + 8)) new_sibling

let child story obj =
  let (Object_address addr) = address story obj in
  if Story.v3_or_lower (Story.version story) then
    Object (Story.read_byte story (Byte_address(addr + 6)))
  else
    Object (Story.read_word story (Word_address(addr + 10)))

let set_child story obj (Object new_child) =
  let (Object_address addr) = address story obj in
  if Story.v3_or_lower (Story.version story) then
    Story.write_byte story (Byte_address (addr + 6)) new_child
  else
    Story.write_word story (Word_address (addr + 10)) new_child

(* The last two bytes in an object description are a pointer to a
block that contains additional properties. *)
let property_header_address story obj =
  let object_property_offset = if Story.v3_or_lower (Story.version story) then 7 else 12 in
  let (Object_address addr) = address story obj in
  Property_header (Story.read_word story (Word_address (addr + object_property_offset)))

(* Oddly enough, the Z machine does not ever say how big the object table is.
   Assume that the address of the first property block in the first object is
   the bottom of the object tree table. *)
let count story =
  let (Object_tree_base table_start) = tree_base story in
  let (Property_header table_end) = property_header_address story (Object 1) in
  let entry_size = entry_size story in
  (table_end - table_start) / entry_size

(* The property entry begins with a length-prefixed zstring *)
let name story n =
  let (Property_header addr) = property_header_address story n in
  let length = Story.read_byte story (Byte_address addr) in
  if length = 0 then "<unnamed>"
  else Zstring.read story (Zstring (addr + 1))

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

(* Takes the address of a property block -- past the string,
pointing to the block header.
Returns the length of the header, the length of the data, and the
property number. *)

let decode_property_data story (Property_address address) =
  let b = Story.read_byte story (Byte_address address) in
  if b = 0 then
    (0, 0, invalid_property)
  else if Story.v3_or_lower (Story.version story) then
    (* In version 3 it's easy. The number of bytes of property data
    is indicated by the top 3 bits; the property number is indicated
    by the bottom 5 bits, and the header is one byte. *)
    (1, (fetch_bits bit7 size3 b) + 1, Property (fetch_bits bit4 size5 b))
  else
    (* In version 4 the property number is the bottom 6 bits. *)
    let prop = Property (fetch_bits bit5 size6 b) in
    (* If the high bit of the first byte is set then the length is
      indicated by the bottom six bits of the *following* byte.
      The following byte needs to have its high bit set as well.
      (See below).

      If the high bit is not set then the length is indicated by
      the sixth bit. *)
    if fetch_bit bit7 b then
      let b2 = Story.read_byte story (Byte_address (address + 1)) in
      let len = fetch_bits bit5 size6 b2 in
      (2, (if len = 0 then 64 else len), prop)
    else
      (1, (if fetch_bit bit6 b then 2 else 1), prop)

(* This method produces a list of (number, data_length, data_address) tuples *)
let property_addresses story obj =
  let rec aux acc address =
    let (Property_address addr) = address in
    let b = Story.read_byte story (Byte_address addr) in
    if b = 0 then
      acc
    else
      let (header_length, data_length, prop) =
        decode_property_data story address in
      let this_property =
        (prop, data_length, Property_data (addr + header_length)) in
      let next_addr = Property_address (addr + header_length + data_length) in
      aux (this_property :: acc) next_addr in
  let (Property_header header) = property_header_address story obj in
  let property_name_address = header in
  let property_name_word_length = Story.read_byte story (Byte_address property_name_address) in
  let first_property_address =
    Property_address (property_name_address + 1 + property_name_word_length * 2) in
  aux [] first_property_address

(* Given the adddress of the data block, how long is it?  In version 3
this is easy; we just look at the previous byte. In version 4 there could
be two bytes before the data, but the one immediately before the data is
always the size byte. If its high bit is on then the bottom six bits are the
size. If the high bit is not on then the size is determined by bit 6. *)
let property_length_from_address story (Property_data address) =
  if address = 0 then
    0
  else
    let b = Story.read_byte story (Byte_address (address - 1)) in
    if Story.v3_or_lower (Story.version story) then
      1 + (fetch_bits bit7 size3 b)
    else
      if fetch_bit bit7 b then
        let len = fetch_bits bit5 size6 b in
        if len = 0 then 64 else len
      else
        if fetch_bit bit6 b then 2 else 1

(* Given an object and property number, what is the address
   of the associated property block? Or zero if there is none. *)
let property_address story obj prop =
  let rec aux addresses =
    match addresses with
    | [] -> invalid_data
    | (number, _, address) :: tail ->
      if number = prop then address
      else aux tail in
  aux (property_addresses story obj)

(* Fetch the one or two byte value associated with a given property of a given object.
If the object does not have that property then fetch the default property value. *)
let property story obj prop =
  (* We simply do a linear search for the property, even though they are
     stored in sorted order. The blocks we are searching are first, variable
     size, which makes them inconvenient to binary search. And second, are
     small, making binary search not worth the bother. *)
  let rec aux addresses =
    match addresses with
    | [] -> default_property_value story prop
    | (number, length, (Property_data address)) :: tail ->
      if number = prop then (
        if length = 1 then
          Story.read_byte story (Byte_address address)
        else if length = 2 then
          Story.read_word story (Word_address address)
        else
          let (Object n) = obj in
          let (Property p) = prop in
          failwith (Printf.sprintf "object %d property %d length %d bad property length" n p length))
      else
        aux tail in
  aux (property_addresses story obj)

(* Given a property number, find the first property of an object
greater than it. Note that this assumes that properties are enumerated in
order by property_addresses. Returns zero if there is no such property. *)
let next_property story obj (Property prop) =
  let rec aux addrs =
    match addrs with
    | [] -> invalid_property
    | (Property number, _, _) :: tail ->
      if number > prop then Property number
      else aux tail in
  aux (property_addresses story obj)

(* Writes a one or two byte property associated with a given object. *)
(* The property must exist and must be one or two bytes. *)
let write_property story obj prop value =
  let rec aux addresses =
    match addresses with
    | [] -> (invalid_data, 0)
    | (number, length, address) :: tail ->
      if number = prop then (address, length)
      else aux tail in
  let (address, length) = aux (property_addresses story obj) in
  if address = invalid_data then failwith "invalid property";
  let (Property_data address) = address in
  match length with
  | 1 -> Story.write_byte story (Byte_address address) value
  | 2 -> Story.write_word story (Word_address address) value
  | _ -> failwith "property cannot be set"

(* Debugging method for displaying the property numbers and
   values for a given object *)
let display_properties story obj =
  let to_string (prop, length, address) =
    let (Property p) = prop in
    let prop_number_text = Printf.sprintf "%02x" p in
    let prop_value_text =
      if length = 1 || length = 2 then
        let prop_value = property story obj prop in
        Printf.sprintf ":%04x " prop_value
      else
        " " in
    prop_number_text ^ prop_value_text in
  let addresses = property_addresses story obj in
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
  let rec aux obj acc =
    let current = Object obj in
    if current = invalid_object then
      acc
    else if (parent story current) = invalid_object then
      aux (obj - 1) (current :: acc)
    else
      aux (obj - 1) acc in
  aux (count story) []

let display_object_tree story =
  let rec aux acc indent obj =
    if obj = invalid_object then
      acc
    else
      let name = name story obj in
      let child = child story obj in
      let sibling = sibling story obj in
      let (Object n) = obj in
      let object_text =
        Printf.sprintf "%s%04d %s\n" indent n name in
      let with_object = acc ^ object_text in
      let new_indent = "    " ^ indent in
      let with_children = aux with_object new_indent child in
      aux with_children indent sibling in
  let to_string obj =
    aux "" "" obj in
  accumulate_strings to_string (roots story)
