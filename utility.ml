open Type

let bit0 = Bit_number 0
let bit1 = Bit_number 1
let bit2 = Bit_number 2
let bit3 = Bit_number 3
let bit4 = Bit_number 4
let bit5 = Bit_number 5
let bit6 = Bit_number 6
let bit7 = Bit_number 7
let bit8 = Bit_number 8
let bit9 = Bit_number 9
let bit10 = Bit_number 10
let bit11 = Bit_number 11
let bit12 = Bit_number 12
let bit13 = Bit_number 13
let bit14 = Bit_number 14
let bit15 = Bit_number 15

let size1 = Bit_size 1
let size2 = Bit_size 2
let size3 = Bit_size 3
let size4 = Bit_size 4
let size5 = Bit_size 5
let size6 = Bit_size 6
let size7 = Bit_size 7

let fetch_bit (Bit_number n) word =
  (word land (1 lsl n)) lsr n = 1

let clear_bit (Bit_number n) word =
  word land (lnot (1 lsl n))

let set_bit (Bit_number n) word =
  word lor (1 lsl n)

let set_bit_to n word value =
  if value then set_bit n word
  else clear_bit n word

let fetch_bits (Bit_number high) (Bit_size length) word =
  let mask = lnot (-1 lsl length) in
  (word lsr (high - length + 1)) land mask

let byte_of_int x =
  x land 0xff

let is_in_range (Byte_address address) size =
    0 <= address && address < size

let is_out_of_range address size =
    not (is_in_range address size)

let inc_byte_addr_by (Byte_address address) offset =
  Byte_address (address + offset)
  
let inc_byte_addr address = 
    inc_byte_addr_by address 1

let dec_byte_addr_by address offset =
  inc_byte_addr_by address (0 - offset)

let word_size = 2

let inc_word_addr_by (Word_address address) offset =
  Word_address (address + offset * word_size)

let inc_word_addr address =
  inc_word_addr_by address 1

let dereference_string address bytes =
  if is_out_of_range address (String.length bytes) then
    failwith "address out of range"
  else
    let (Byte_address addr) = address in
    int_of_char bytes.[addr]

let address_of_high_byte (Word_address address) =
  Byte_address address

let address_of_low_byte (Word_address address) =
  Byte_address (address + 1)

let get_file filename =
  let channel = open_in_bin filename in
  let length = in_channel_length channel in
  let file = really_input_string channel length in
  close_in channel;
  file

let string_of_char c =
  String.make 1 c
  
let accumulate_strings_loop to_string start max =
  let rec aux acc i =
    if i >= max then acc
    else aux (acc ^ (to_string i)) (i + 1) in
  aux "" start
  
let accumulate_strings to_string items =
  let folder text item =
    text ^ (to_string item) in
  List.fold_left folder "" items

let unsigned_word word =
  ((word mod 65536) + 65536) mod 65536

let signed_word word =
  let canonical = unsigned_word word in
  if canonical > 32767 then canonical - 65536 else canonical

let byte_addr_to_word_addr (Byte_address address) =
  Word_address address
  
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
