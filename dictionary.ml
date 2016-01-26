open Story
open Utility

type dictionary_address =
  Address of int

let invalid_address = Address 0

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

let entry_length story =
  let base = dictionary_base story in
  let separators = word_separators_count story in
  read_byte story (base + separators + 1)

let max_word_length story =
  if (version story) <= 3 then 6 else 9

let entry_count story =
  let base = dictionary_base story in
  let separators = word_separators_count story in
  read_word story (base + separators + 2)

let table_base story =
  let base = dictionary_base story in
  let separators = word_separators_count story in
  base + separators + 4

let entry_address story (Dictionary dictionary_number) =
  let base = table_base story in
  let entry_length = entry_length story in
  Address (base + dictionary_number * entry_length)

let entry story dictionary_number =
  let (Address addr) = entry_address story dictionary_number in
  read_zstring story (Zstring.Address addr)

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
