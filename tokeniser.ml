open Type

type token =
{
  token_text : string;
  start : int; (* Offset in the input text *)
  dictionary_address : dictionary_address
}

(* TODO: Get word separator list from story *)

(* Returns a list of tuples containing each word, the start location
in the input string and the address of the matching dictionary word. *)
let tokenise story text =
  let length = String.length text in

  let rec find_space_or_end i =
    if i = length then i
    else if text.[i] = ' ' then i
    else find_space_or_end (i + 1) in

  let rec skip_spaces i =
    if i = length then i
    else if text.[i] = ' ' then skip_spaces (i + 1)
    else i in

  let find_token start =
    if start = length then
      None
    else
      let end_of_token = find_space_or_end start in
      let token_text = String.sub text start (end_of_token - start) in
      let dictionary_address = Dictionary.lookup story token_text in
      Some {token_text; start; dictionary_address} in

  let rec aux i acc =
    match find_token i with
    | None -> acc
    | Some ({token_text; start; dictionary_address} as token) ->
      let token_length = String.length token_text in
      let next_non_space = skip_spaces (i + token_length) in
      let new_acc = token :: acc in
      aux next_non_space new_acc in

  List.rev (aux (skip_spaces 0) [])
    (* End of tokenise*)

(* TODO: What is the type of address? *)
let write_tokens items address max_parse story =

  (* Spec:
  * one 4-byte block is written for each word
  * it should stop before going beyond the maximum number of words specified.
  * Each block consists of the byte address of the word in the dictionary,
    if it is in the dictionary, or 0 if it isn't; followed by a byte giving
    the number of letters in the word; and finally a byte giving the position
    in the text-buffer of the first letter of the word. *)

  let text_buffer_offset = if Story.v4_or_lower (Story.version story) then 1 else 2 in
  let rec aux items address count story =
    match items with
    | [] -> story
    | {token_text; start; dictionary_address} :: tail ->
      if count = max_parse then
        story
      else
        let (Dictionary_address dictionary_address) = dictionary_address in
        let story = Story.write_word story (Word_address address) dictionary_address in
        let story = Story.write_byte story (Byte_address (address + 2)) (String.length token_text) in
        let story = Story.write_byte story (Byte_address (address + 3)) (start + text_buffer_offset) in
        aux tail (address + 4) (count + 1) story in
  aux items address 0 story

(* TODO: This is wrong; we need to pass in not the trimmed string but the
buffer the trimmed string was written to, that the whole thing may be
parsed again. This will be necessary in order to implement the tokenise
instruction. *)
let write_user_string_to_memory story (Input_buffer text_addr) trimmed =
  (* Now we have to write the string into story memory. This is a bit tricky. *)
  if Story.v4_or_lower (Story.version story) then
    (* Spec: In Versions 1 to 4, ...  stored in bytes 1 onward, with a zero
    terminator (but without any other terminator, such as a carriage return code).
    ----
    This seems straighforward. We write the string starting at byte one,
    and terminate it with a zero. *)
    Story.write_string_zero_terminate story (Sz_address (text_addr + 1)) trimmed
  else
    (* Spec: In Versions 5 and later, ... the interpreter stores the number of
    characters actually typed in byte 1 (not counting the terminating character),
    and the characters themselves in bytes 2 onward (not storing the
    terminating character).
      Moreover, if byte 1 contains a positive value at the start of the input,
    then read assumes that number of characters are left over from an
    interrupted previous input, and writes the new characters
    after those already there. Note that the interpreter does not redisplay
    the characters left over: the game does this, if it wants to.
    ---
    This part of the specification is again, very confusingly written. I think
    the correct interpretation for version 5 is:
    * the text buffer consists of two lead bytes and L characters.
    * the maximum number of user-supplied characters which may *ever*
      be written is L, and it is stored in byte 0
    * the number of user-supplied characters *currently* written
      is C and is in byte 1
    * When writing new text in, the new text is written in starting
      C + 2 bytes from the first lead byte. The second lead byte is
      then updated so that it always contains the actual number of
      user-supplied characters in the buffer. *)
      let current_letters = Story.read_byte story (Byte_address (text_addr + 1)) in
      let story = Story.write_string story (String_address (text_addr + current_letters + 2)) trimmed in
      let length = String.length trimmed in
      Story.write_byte story (Byte_address (text_addr + 1)) (current_letters + length)

(* TODO: Could use some helpers here to better type the parse buffer *)
let lexical_analysis story (Parse_buffer parse_addr) trimmed =
  (* Spec:
   Initially, byte 0  of the parse-buffer should hold the maximum
   number of textual words which can be parsed. (If this is n, the buffer
   must be at least 2 + 4*n bytes long to hold the results of the analysis.*)
  let maximum_parse = Story.read_byte story (Byte_address parse_addr) in
  (* Spec: The interpreter divides the text into words and looks them up in the
     dictionary, as described in section 13. *)
  let tokens = tokenise story trimmed in
  (* Spec: The number of words is written in byte 1 *)
  let count = min maximum_parse (List.length tokens) in
  let story = Story.write_byte story (Byte_address (parse_addr + 1)) count in
  (* Spec: one 4-byte block is written for each word, from byte 2 onwards
  (except that it should stop before going beyond the maximum number of words
  specified). *)
  write_tokens tokens (parse_addr + 2) maximum_parse story
