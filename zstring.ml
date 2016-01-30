open Utility
open Type

let abbreviation_table_length = 96

(* A "word address" is only used in the abbreviation table, and is always
just half the real address. A "packed address" is used in calls and fetching
strings, and is half the real address in v3 but different for other versions. *)

let decode_word_address (Word_zstring word_address) =
  Zstring (word_address * 2)

let first_abbrev_addr (Abbreviation_table_base base) =
  Word_address base

let abbreviation_zstring story (Abbreviation n) =
  if n < 0 || n >= abbreviation_table_length then
    failwith "bad offset into abbreviation table"
  else
    let base = first_abbrev_addr (Story.abbreviations_table_base story) in
    let abbr_addr = inc_word_addr_by base n in
    let word_addr = Word_zstring (Story.read_word story abbr_addr) in
    decode_word_address word_addr

let alphabet_table = [|
  "_"; "?"; "?"; "?"; "?"; "?"; "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j";
  "k"; "l"; "m"; "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z" |]

let display_bytes story (Zstring addr) =
  let rec aux current acc =
    let word = Story.read_word story current in
    let is_end = fetch_bits bit15 size1 word in
    let zchar1 = fetch_bits bit14 size5 word in
    let zchar2 = fetch_bits bit9 size5 word in
    let zchar3 = fetch_bits bit4 size5 word in
    let s = Printf.sprintf "%02x %s %02x %s %02x %s "
      zchar1 alphabet_table.(zchar1) 
      zchar2 alphabet_table.(zchar2) 
      zchar3 alphabet_table.(zchar3) in
    let acc = acc ^ s in
    if is_end = 1 then acc
    else aux (inc_word_addr current) acc in
  aux (Word_address addr) ""
