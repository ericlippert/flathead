open Utility
open Type

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

let parent story obj =
  let (Object_address addr) = address story obj in
  if Story.v3_or_lower (Story.version story) then
    Object (Story.read_byte story (Byte_address (addr + 4)))
  else
    Object (Story.read_word story (Word_address (addr + 6)))

let sibling story obj =
  let (Object_address addr) = address story obj in
  if Story.v3_or_lower (Story.version story) then
    Object (Story.read_byte story (Byte_address (addr + 5)))
  else
    Object (Story.read_word story (Word_address (addr + 8)))

let child story obj =
  let (Object_address addr) = address story obj in
  if Story.v3_or_lower (Story.version story) then
    Object (Story.read_byte story (Byte_address(addr + 6)))
  else
    Object (Story.read_word story (Word_address(addr + 10)))

(* The last two bytes in an object description are a pointer to a
block that contains additional properties. *)
let property_header_address story obj =
  let object_property_offset =
    if Story.v3_or_lower (Story.version story) then 7 else 12 in
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

let display_object_table story =
  let count = count story in
  let to_string i =
    let current = Object i in
    let (Object parent) = parent story current in
    let (Object sibling) = sibling story current in
    let (Object child) = child story current in
    let name = name story current in
    Printf.sprintf "%02x: %02x %02x %02x %s\n"
      i parent sibling child name in
  accumulate_strings_loop to_string 1 (count + 1)
