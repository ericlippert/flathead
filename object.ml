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

let invalid_object = Object 0

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
      let object_text =
        Printf.sprintf "%s%s\n" indent name in
      let with_object = acc ^ object_text in
      let new_indent = "    " ^ indent in
      let with_children = aux with_object new_indent child in
      aux with_children indent sibling in
  let to_string obj =
    aux "" "" obj in
  accumulate_strings to_string (roots story)
