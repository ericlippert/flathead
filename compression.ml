open Type
open Utility

let compress story =
  let original_story = Story.original story in
  let (Static_memory_base memory_length) = Story.static_memory_base story in
  let rec aux acc i c =
    if i = memory_length then
      acc
    else if c = 256 then
      let encoded = "\000" ^ (string_of_byte (c - 1)) in
      aux (acc ^ encoded) i 0
    else
      let original_byte = Story.read_byte original_story (Byte_address i) in
      let current_byte = Story.read_byte story (Byte_address i) in
      let combined = original_byte lxor current_byte in
      if combined = 0 then
        aux acc (i + 1) (c + 1)
      else if c > 0 then
        let encoded = "\000" ^ (string_of_byte (c - 1)) ^ (string_of_byte combined) in
        aux (acc ^ encoded) (i + 1) 0
      else
        let encoded = string_of_byte combined in
        aux (acc ^ encoded) (i + 1) 0 in
  Compressed (aux "" 0 0)

let apply_uncompressed_changes story (Uncompressed uncompressed) =
  (* We cannot simply say "make a new dynamic memory chunk out of
  these bytes" because then the *next* time we load a save game,
  that dynamic memory will be the "original" memory, which is wrong.
  We need to maintain the truly original loaded-off-disk memory. *)
  let original_story = Story.original story in
  let length = String.length uncompressed in
  let rec aux index acc =
    if index >= length then
      acc
    else
      let new_byte = int_of_char uncompressed.[index] in
      let orig_byte = Story.read_byte original_story (Byte_address index) in
      let new_story =
        if new_byte = orig_byte then acc
        else Story.write_byte acc (Byte_address index) new_byte in
      aux (index + 1) new_story in
  aux 0 original_story

let apply_compressed_changes story (Compressed compressed) =
  let original_story = Story.original story in
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
        let orig_byte = Story.read_byte original_story (Byte_address index_mem) in
        let new_byte = b lxor orig_byte in
        let new_story = Story.write_byte acc (Byte_address index_mem) new_byte in
        aux (index_change + 1) (index_mem + 1) new_story in
  aux 0 0 original_story
