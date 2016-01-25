open Utility

type iff_contents =
| Header of string
| SubHeader of string
| Length of int option
| RemainingBytes of string option
| ByteString of (string option) * int
| Integer32 of int option
| Integer24 of int option
| Integer16 of int option
| Integer8 of int option
| Integer4 of int option
| Bit of int * (bool option)
| BitField of iff_contents list
| Record of iff_contents list
| Assign of string * iff_contents
| Lookup of string
| SizedList of iff_contents * (iff_contents list)
| UnsizedList of iff_contents list
| UnorderedList of iff_contents list

exception BadFileFormat
(* TODO: Better error handling *)



let read_iff_file filename root_form =

  let rec remove_assign form =
    match form with
    | Assign (_, f) -> remove_assign f
    | BitField contents -> BitField (List.map remove_assign contents)
    | Record contents -> Record (List.map remove_assign contents)
    | SizedList (n, contents) -> SizedList (n, List.map remove_assign contents)
    | UnsizedList contents -> UnsizedList (List.map remove_assign contents)
    | UnorderedList contents -> UnorderedList (List.map remove_assign contents)
    | _ -> form in

  let file = get_file filename in

  let rec read_form offset form end_position context =
    let rec resolve_lookup name forms =
      (* Does not recurse into records. It could. *)
      match forms with
      | [] -> None
      | (Assign (n, v)) :: tail ->
        if n = name then Some v
        else resolve_lookup name tail
      | (BitField bitfields) :: tail ->
        (match resolve_lookup name bitfields with
        | None -> resolve_lookup name tail
        | result -> result)
      | _ :: tail -> resolve_lookup name tail in

    let read_string_from_file offset length =
      if offset + length > end_position then raise BadFileFormat
      else String.sub file offset length in

    let read_int8_from_file offset =
      if offset >= end_position then raise BadFileFormat
      else int_of_char file.[offset] in

    let read_int32_from_file offset =
      if offset + 4 > end_position then
        raise BadFileFormat
      else
        let b3 = read_int8_from_file offset in
        let b2 = read_int8_from_file (offset + 1) in
        let b1 = read_int8_from_file (offset + 2) in
        let b0 = read_int8_from_file (offset + 3) in
        b0 + 256 * b1 + 256 * 256 * b2 + 256 * 256 * 256 * b3 in

    let peek_chunk offset =
      (read_string_from_file offset 4, read_int32_from_file (offset + 4)) in

    let rec form_to_integer form =
      match form with
      | Length (Some n) -> n
      | Integer32 (Some n) -> n
      | Integer24 (Some n) -> n
      | Integer16 (Some n) -> n
      | Integer8 (Some n) -> n
      | Integer4 (Some n) -> n
      | Assign (_, f) -> form_to_integer f
      | Lookup s -> failwith "TODO: Lookup not implemented"
      | _ -> failwith "form does not have value" in

    let read_header id =
      let header = read_string_from_file offset 4 in
      if header = id then (Header id, offset + 4)
      else raise BadFileFormat in

    let read_subheader id =
      let header = read_string_from_file offset 4 in
      if header = id then (SubHeader id, offset + 4)
      else raise BadFileFormat in

    let read_length () =
      let length = read_int32_from_file offset in
      (Length (Some length), offset + 4) in

    let read_remaining_bytes () =
      let bytes = read_string_from_file offset (end_position - offset) in
      (RemainingBytes (Some bytes), end_position) in

    let read_byte_string length =
      let bytes = read_string_from_file offset length in
      (ByteString ((Some bytes), length), offset + length) in

    let read_int32 () =
      let n = read_int32_from_file offset in
      (Integer32 (Some n), offset + 4) in

    let read_int24 () =
      let b2 = read_int8_from_file offset in
      let b1 = read_int8_from_file (offset + 1) in
      let b0 = read_int8_from_file (offset + 2) in
      (Integer24 (Some (b0 + (256 * b1) + (256 * 256 * b2))), offset + 3) in

    let read_int16 () =
      let b1 = read_int8_from_file offset in
      let b0 = read_int8_from_file (offset + 1) in
      (Integer16 (Some (b0 + (256 * b1))), offset + 2) in

    let read_int8 () =
      let b0 = read_int8_from_file offset in
      (Integer8 (Some b0), offset + 1) in

    let read_record forms =
      let (new_end_position, skip_byte) =
        match forms with
        | (Header _) :: (Length _) :: tail ->
          let (_, length) = peek_chunk offset in
          (offset + 8 + length, length mod 2 != 0)
        | _ -> (end_position, false) in
      let process acc form =
        let (acc_context, current_offset, acc_results) = acc in
        let (new_result, new_offset) = read_form current_offset form new_end_position acc_context in
        (new_result :: acc_context, new_offset, new_result :: acc_results) in
      let (_, new_offset, results) = List.fold_left process (context, offset, []) forms in
        (* If a record has an odd length then there will always be
        an extra 0 byte unaccounted for at the end. Skip it. *)
      let adjusted = if skip_byte then new_offset + 1 else new_offset in
      (Record (List.rev results), adjusted) in

    let read_bitfield fields =
      let byte = read_int8_from_file offset in
      let fetch_bit n =
        (byte land (1 lsl n)) lsr n = 1 in
      let rec process_field field =
        match field with
        | Bit (n, _) -> Bit (n, Some (fetch_bit n))
        | Integer4 _ -> Integer4 (Some (byte land 0xF) )
        | Assign (name, f) -> Assign (name, process_field f)
        | _ -> failwith "pattern unexpected in bit field" in
      (BitField (List.map process_field fields), offset + 1) in

    let read_unsized_list forms =
      match forms with
      | [ form ] ->
        let rec aux acc current_offset =
          if current_offset >= end_position then (acc, current_offset)
          else
            let (new_form, new_offset) = read_form current_offset form end_position context in
            aux (new_form :: acc) new_offset in
        let (new_forms, new_offset) = aux [] offset in
        (UnsizedList (List.rev new_forms), new_offset)
      | _ -> failwith "unexpected pattern in unsized list" in

    let read_sized_list size forms =
      let (s, size_offset) = read_form offset size end_position context in
      let n = form_to_integer s in
      match forms with
      | [form] ->
        let rec aux acc i current_offset =
          if i = 0 then (acc, current_offset)
          else
            let (new_form, new_offset) = read_form current_offset form end_position context in
            aux (new_form :: acc) (i - 1) new_offset in
      let (new_forms, new_offset) = aux [] n offset in
      (SizedList (s, List.rev new_forms), new_offset)
      | _ -> failwith "unexpected pattern in sized list" in

    let read_unordered_list forms =
      (* We have a collection of expected chunks; we need to skip
      unexpected chunks. *)
      let rec aux acc current_offset =
        if current_offset >= end_position then (acc, current_offset)
        else
          let (header, length) = peek_chunk current_offset in
          let predicate form =
            match form with
            | Record ((Header id) :: tail) -> header = id
            | _ -> false in
          match first_match forms predicate with
          | None ->
            let new_offset = current_offset + 8 + length + (if length mod 2 = 0 then 0 else 1) in
            let new_form = Record [ Header header; Length (Some length); RemainingBytes None] in
            aux (new_form :: acc) new_offset
          | Some form ->
            let (new_form, new_offset) = read_form current_offset form end_position context in
            aux (new_form :: acc) new_offset in
      let (new_forms, new_offset) = aux [] offset in
      (UnorderedList (List.rev new_forms), new_offset) in

    match form with
    | Header id -> read_header id
    | Length _ -> read_length ()
    | Bit _ -> failwith "Bit only expected inside bitfield"
    | Integer4 _ -> failwith "Integer4 only expected inside bitfield"
    | SubHeader id -> read_subheader id
    | RemainingBytes _ -> read_remaining_bytes ()
    | ByteString (_, length) -> read_byte_string length
    | Integer32 _ -> read_int32 ()
    | Integer24 _ -> read_int24 ()
    | Integer16 _ -> read_int16 ()
    | Integer8 _ -> read_int8 ()
    | BitField fields -> read_bitfield fields
    | Record forms -> read_record forms
    | UnsizedList forms -> read_unsized_list forms
    | Assign (name, f) ->
      let (new_form, new_offset) = read_form offset f end_position context in
      (Assign (name, new_form), new_offset)
    | Lookup name ->
      (match resolve_lookup name context with
      | Some f -> (f, offset)
      | None -> failwith "Could not resolve lookup")
    | SizedList (size, forms) -> read_sized_list size forms
    | UnorderedList forms -> read_unordered_list forms in
    (* end of read_form *)
  let (form, _) = read_form 0 root_form (String.length file) [] in
  remove_assign form
  (* end of read_iff_file *)

let write_iff_file filename root_form =
  let rec write_form form =
    let write_int32_to_file n =
      let b0 = string_of_char (char_of_int (n land 0xff)) in
      let b1 = string_of_char (char_of_int ((n asr 8 ) land 0xff)) in
      let b2 = string_of_char (char_of_int ((n asr 16) land 0xff)) in
      let b3 = string_of_char (char_of_int ((n asr 24) land 0xff)) in
      b3 ^ b2 ^ b1 ^ b0 in

    let write_int24_to_file n =
      let b0 = string_of_char (char_of_int (n land 0xff)) in
      let b1 = string_of_char (char_of_int ((n asr 8 ) land 0xff)) in
      let b2 = string_of_char (char_of_int ((n asr 16) land 0xff)) in
      b2 ^ b1 ^ b0 in

    let write_int16_to_file n =
      let b0 = string_of_char (char_of_int (n land 0xff)) in
      let b1 = string_of_char (char_of_int ((n asr 8 ) land 0xff)) in
      b1 ^ b0 in

    let write_int8_to_file n =
      string_of_char (char_of_int (n land 0xff)) in

    let write_bitfield fields =
      let rec process_field field =
        match field with
        | Integer4 (Some n) -> n land 0xf
        | Bit (n, (Some flag)) -> if flag then 1 lsl n else 0
        | Assign (_, named_field) -> process_field named_field
        | _ -> failwith "unexpected form in bitfield" in
      let folder b field =
        b lor (process_field field) in
      let v = List.fold_left folder 0 fields in
      string_of_char (char_of_int v) in

    let write_many s form =
      s ^ (write_form form) in

    let write_record forms =
      match forms with
      | (Header header) :: (Length _) :: tail ->
        let body = List.fold_left write_many "" tail in
        let length = String.length body in
        let chunk = header ^ (write_int32_to_file length) ^ body in
        let adjusted = if length mod 2 = 0 then chunk else chunk ^ "\000" in
        adjusted
      | _ -> List.fold_left write_many "" forms in

    match form with
    | Header header -> header
    | SubHeader subheader -> subheader
    | Length (Some length) -> write_int32_to_file length
    | RemainingBytes (Some bytes) -> bytes
    | ByteString ((Some bytes), _) -> bytes
    | Integer32 (Some n) -> write_int32_to_file n
    | Integer24 (Some n) -> write_int24_to_file n
    | Integer16 (Some n) -> write_int16_to_file n
    | Integer8 (Some n) -> write_int8_to_file n
    | Integer4 _ -> failwith "expected Integer4 inside bitfield"
    | Bit _ -> failwith "expected Bit inside bitfield"
    | BitField fields -> write_bitfield fields
    | Assign (_, named_form) -> write_form named_form
    | SizedList (_, forms) -> List.fold_left write_many "" forms
    | UnsizedList forms -> List.fold_left write_many "" forms
    | UnorderedList forms -> List.fold_left write_many "" forms
    | Record forms -> write_record forms
    | _ -> failwith "unexpected form in write_form" in
  (* end of write_form *)
  let text = write_form root_form in
  write_file filename text
  (* end of write_iff_file *)

let rec find_record chunks target =
  match chunks with
  | [] -> None
  | (Record (Header header :: _) as record) :: tail ->
    if header = target then Some record
    else find_record tail target
  | _ :: tail -> find_record tail target
