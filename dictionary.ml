open Utility
open Type

let invalid_address = Dictionary_address 0

(* The table is laid out as follows. First there is a header:

byte giving the number of word separators
the word separators, one byte each
byte giving the number of bytes in each dictionary entry
word giving the number of table entries which follow

Each entry is either 4 (in V1-3) or 6 (otherwise) bytes of zstring data,
followed by enough bytes to make up the size of the dictionary entry. *)

let max_word_length story =
  if Story.v3_or_lower (Story.version story) then 6 else 9

let word_separators_base (Dictionary_base base) =
  Byte_address base

let word_separator_address base (Word_separator_number n) =
  let ws_base = word_separators_base base in
  inc_byte_addr_by ws_base (n + 1)

let word_separators_count story =
  let dict_base = Story.dictionary_base story in
  let ws_base = word_separators_base dict_base in
  Story.read_byte story ws_base

let word_separators story =
  let dict_base = Story.dictionary_base story in
  let count = word_separators_count story in
  let rec aux acc i =
    if i = count then
      acc
    else
      let addr = word_separator_address dict_base (Word_separator_number i) in
      let separator = string_of_char (char_of_int (Story.read_byte story addr)) in
      aux (separator :: acc) (i + 1) in
  aux [] 0

let entry_base story =
  let dict_base = Story.dictionary_base story in
  let ws_count = word_separators_count story in
  let ws_base = word_separators_base dict_base in
  inc_byte_addr_by ws_base (ws_count + 1)

let entry_length story =
  Story.read_byte story (entry_base story)

let entry_count story =
  let (Byte_address addr) = inc_byte_addr (entry_base story) in
  Story.read_word story (Word_address addr)

(* This is the address of the actual dictionary entries, past the initial
header with the word separators. *)
let table_base story =
  let (Byte_address addr) = inc_byte_addr_by (entry_base story) 3 in
  Dictionary_table_base addr

let entry_address story (Dictionary dictionary_number) =
  let (Dictionary_table_base base) = table_base story in
  let length = entry_length story in
  Dictionary_address (base + dictionary_number * length)

let entry story dictionary_number =
  let (Dictionary_address addr) = entry_address story dictionary_number in
  Zstring.read story (Zstring addr)

(* Takes a string and finds the address of the corresponding zstring
  in the dictionary *)
(* Note this computes the address of the dictionary string, not the dictionary
  entry number. *)

(* This returns zero if the string cannot be found. Of course zero is a valid
address in the Z-machine; it's the location of the version number. But it
is conventionally used here as an invalid address. *)
let lookup story text =
  let count = entry_count story in
  let truncated = truncate text (max_word_length story) in
  let compare i =
    String.compare (entry story (Dictionary i)) truncated in
  match binary_search 0 count compare with
  | None -> invalid_address
  | Some entry_index -> entry_address story (Dictionary entry_index)

let display_dictionary story =
  let count = entry_count story in
  let header =
    (Printf.sprintf "Separator count: %d\n" (word_separators_count story)) ^
    (Printf.sprintf "Entry length:    %d\n" (entry_length story)) ^
    (Printf.sprintf "Entry count:     %d\n" count) in
  let to_string i =
    Printf.sprintf "%04x: %s\n" i (entry story (Dictionary i)) in
  header ^ (accumulate_strings_loop to_string 0 count)
