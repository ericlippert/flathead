open Story

type token =
{
  token_text : string;
  start : int; (* Offset in the input text *)
  dictionary_address : int (* Address in the dictionary *)
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
      let dictionary_address = dictionary_lookup story token_text in
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

let write_tokens items address max_parse story =

  (* Spec:
  * one 4-byte block is written for each word
  * it should stop before going beyond the maximum number of words specified.
  * Each block consists of the byte address of the word in the dictionary,
    if it is in the dictionary, or 0 if it isn't; followed by a byte giving
    the number of letters in the word; and finally a byte giving the position
    in the text-buffer of the first letter of the word. *)

  let text_buffer_offset = if (version story) <= 4 then 1 else 2 in
  let rec aux items address count story =
    match items with
    | [] -> story
    | {token_text; start; dictionary_address} :: tail ->
      if count = max_parse then
        story
      else
        let story = write_word story address dictionary_address in
        let story = write_byte story (address + 2) (String.length token_text) in
        let story = write_byte story (address + 3) (start + text_buffer_offset) in
        aux tail (address + 4) (count + 1) story in
  aux items address 0 story
