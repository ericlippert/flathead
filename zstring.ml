open Utility

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
let length word_reader address =
  let rec aux len current =
    if fetch_bit bit15 (word_reader current) then len + 2
    else aux (len + 2) (current + 2) in
  aux 0 address

let rec read word_reader abbrv_reader address =
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
    | (Abbreviation a, _) -> (abbrv_reader (a + zchar), Alphabet 0)
    | (Leading, _) -> ("", (Trailing zchar))
    | (Trailing high, _) ->
      let s = string_of_char (Char.chr (high * 32 + zchar)) in
      (s, Alphabet 0) in

  let rec aux acc mode1 current_address =
    let zchar_bit_size = size5 in
    let word = word_reader current_address in
    let is_end = fetch_bit bit15 word in
    let zchar1 = fetch_bits bit14 zchar_bit_size word in
    let zchar2 = fetch_bits bit9 zchar_bit_size word in
    let zchar3 = fetch_bits bit4 zchar_bit_size word in
    let (text1, mode2) = process_zchar zchar1 mode1 in
    let (text2, mode3) = process_zchar zchar2 mode2 in
    let (text3, mode_next) = process_zchar zchar3 mode3 in
    let new_acc = acc ^ text1 ^ text2 ^ text3 in
    if is_end then new_acc
    else aux new_acc mode_next (current_address + 2) in
  aux "" (Alphabet 0) address

(* A debugging method for looking at memory broken up into the
1 / 5 / 5 / 5 bit chunks used by zstrings. *)

let display_bytes word_reader offset length =
  let rec aux i acc =
    if i > length then acc
    else (
      let word = word_reader (offset + i) in
      let is_end = fetch_bits bit15 size1 word in
      let zchar1 = fetch_bits bit14 size5 word in
      let zchar2 = fetch_bits bit9 size5 word in
      let zchar3 = fetch_bits bit4 size5 word in
      let s = Printf.sprintf "%04x(%01x %02x %02x %02x) " word is_end zchar1 zchar2 zchar3 in
      aux (i + 2) (acc ^ s)) in
    aux 0 ""
