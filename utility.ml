(* Just some useful stuff that has no good home *)

module IntMap = Map.Make(struct type t = int let compare = compare end)

let string_of_char x =
  String.make 1 x

let truncate text length =
  if (String.length text) > length then String.sub text 0 length
  else text

let rec times f n item =
  if n = 0 then item else times f (n - 1) (f item)

let spaces n =
  String.make n ' '

let rec reverse_index_from text target index =
  if index < 0 then None
  else if text.[index] = target then Some index
  else reverse_index_from text target (index - 1)

let left_string text length =
  String.sub text 0 length

let right_string text index =
  String.sub text index ((String.length text) - index)

let break_string text target =
  let index = String.index text target in
  let left = left_string text index in
  let right = right_string text (index + 1) in
  (left, right)

let replace_at original_text index new_text =
  let len = String.length new_text in
  let left = left_string original_text index in
  let right = right_string original_text (index + len) in
  left ^ new_text ^ right

let accumulate_strings to_string items =
  let folder text item =
    text ^ (to_string item) in
  List.fold_left folder "" items

let accumulate_strings_loop to_string start max =
  let rec aux acc i =
    if i >= max then acc
    else aux (acc ^ (to_string i)) (i + 1) in
  aux "" start

let unsigned_word word =
  ((word mod 65536) + 65536) mod 65536

let signed_word word =
  let canonical = unsigned_word word in
  if canonical > 32767 then canonical - 65536 else canonical

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

let fetch_bit n word =
  (word land (1 lsl n)) lsr n = 1

let clear_bit n word =
  word land (lnot (1 lsl n))

let set_bit n word =
  word lor (1 lsl n)

let set_bit_to n word value =
  if value then set_bit n word
  else clear_bit n word

let fetch_bits high length word =
  let mask = lnot (-1 lsl length) in
  (word lsr (high - length + 1)) land mask

let display_bytes get_byte first length =
  let blocksize = 16 in
  let to_string i =
    let header =
      if i mod blocksize = 0 then Printf.sprintf "\n%06x: " i
      else "" in
    let byte = get_byte i in
    let contents = Printf.sprintf "%02x " byte in
    header ^ contents in
  (accumulate_strings_loop to_string first (first + length)) ^ "\n"

let really_input_string channel length =
  let bytes = String.create length in
  really_input channel bytes 0 length;
  bytes

let get_file filename =
  let channel = open_in_bin filename in
  let length = in_channel_length channel in
  let file = really_input_string channel length in
  close_in channel;
  file

let write_file filename text =
  let channel = open_out_bin filename in
  output_string channel text;
  close_out channel

let rec first_match items predicate =
  match items with
  | h :: t -> if predicate h then Some h else first_match t predicate
  | [] -> None
