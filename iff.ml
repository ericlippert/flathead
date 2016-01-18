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
| UnorderedList of iff_contents list ;;

exception BadFileFormat;;

let really_input_string channel length =
  let bytes = String.create length in
  really_input channel bytes 0 length;
  bytes;;

let rec first_match items predicate =
  match items with
  | h :: t -> if predicate h then Some h else first_match t predicate
  | [] -> None;;

let read_iff_file filename form =
  let channel = open_in_bin filename in
  let rec read_form form end_position context =

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

    let peek_chunk () =
      let position = pos_in channel in
      let header = really_input_string channel 4 in
      let length = input_binary_int channel in
      seek_in channel position;
      (header, length) in

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
      let header = really_input_string channel 4 in
      if header = id then Header id
      else raise BadFileFormat in

    let read_subheader id =
      let header = really_input_string channel 4 in
      if header = id then SubHeader id
      else raise BadFileFormat in

    let read_length () =
      Length (Some (input_binary_int channel)) in

    let read_remaining_bytes () =
      let position = pos_in channel in
      let bytes = really_input_string channel (end_position - position) in
      RemainingBytes (Some bytes) in

    let read_byte_string length =
      let bytes = really_input_string channel length in
      ByteString ((Some bytes), length) in

    let read_int32 () =
      Integer32 (Some (input_binary_int channel)) in

    let read_int24 () =
      let b2 = input_byte channel in
      let b1 = input_byte channel in
      let b0 = input_byte channel in
      Integer24 (Some (b0 + (256 * b1) + (256 * 256 * b2))) in

    let read_int16 () =
      let b1 = input_byte channel in
      let b0 = input_byte channel in
      Integer16 (Some (b0 + (256 * b1))) in

    let read_int8 () =
      let b0 = input_byte channel in
      Integer8 (Some b0) in

    let read_record forms =
      let (new_end_position, skip_byte) =
        match forms with
        | (Header _) :: (Length _) :: tail ->
          let (_, length) = peek_chunk () in
          let position = pos_in channel in
          (position + 8 + length, length mod 2 != 0)
        | _ -> (end_position, false) in
      let process acc form =
        let (acc_context, acc_results) = acc in
        let new_result = read_form form new_end_position acc_context in
        (new_result :: acc_context, new_result :: acc_results) in
      let (_, results) = List.fold_left process (context, []) forms in
        (* If a record has an odd length then there will always be
        an extra 0 byte unaccounted for at the end. Skip it. *)
      Printf.printf "current position: %04x end position %04x skipping: %b \n" (pos_in channel) new_end_position skip_byte; flush stdout;
      if skip_byte then
        seek_in channel (new_end_position + 1);
      Record (List.rev results) in

    let read_bitfield fields =
      let byte = input_byte channel in
      let fetch_bit n =
        (byte land (1 lsl n)) lsr n = 1 in
      let rec process_field field =
        match field with
        | Bit (n, _) -> Bit (n, Some (fetch_bit n))
        | Integer4 _ -> Integer4 (Some (byte land 0xF) )
        | Assign (name, f) -> Assign (name, process_field f)
        | _ -> failwith "pattern unexpected in bit field" in
      BitField (List.map process_field fields) in

    let read_unsized_list forms =
      match forms with
      | [ form ] ->
        let rec aux acc =
          let position = pos_in channel in
          if position >= end_position then acc
          else aux ((read_form form end_position context) :: acc) in
        UnsizedList (List.rev (aux []))
      | _ -> failwith "unexpected pattern in unsized list" in

    let read_sized_list size forms =
      let s = read_form size end_position context in
      let n = form_to_integer s in
      match forms with
      | [form] ->
        let rec aux acc i =
          if i = 0 then acc
          else aux ((read_form form end_position context) :: acc) (i - 1) in
        SizedList (s, List.rev (aux [] n))
      | _ -> failwith "unexpected pattern in sized list" in

    let read_unordered_list forms =
      (* We have a collection of expected chunks; we need to skip
      unexpected chunks. *)
      let rec aux acc =
        let position = pos_in channel in
        if position >= end_position then acc
        else
          let (header, length) = peek_chunk () in
          let predicate form =
            match form with
            | Record ((Header id) :: tail) -> header = id
            | _ -> false in
          match first_match forms predicate with
          | None ->
            (let skip = 8 + length + (if length mod 2 = 0 then 0 else 1) in
            seek_in channel (position + skip);
            let new_form = Record [ Header header; Length (Some length); RemainingBytes None] in
            aux  (new_form :: acc))
          | Some form ->
            let new_form = read_form form end_position context in
            aux (new_form :: acc) in
      UnorderedList (List.rev (aux [])) in

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
    | Assign (name, f) -> Assign (name, read_form f end_position context)
    | Lookup name ->
      (match resolve_lookup name context with
      | Some f -> f
      | None -> failwith "Could not resolve lookup")
    | SizedList (size, forms) -> read_sized_list size forms
    | UnorderedList forms -> read_unordered_list forms in
    (* end of read_form *)

  let length = in_channel_length channel in
  let result = read_form form length [] in
  close_in channel;
  result;;





(*
let ifzs_intd =
  Record [
    Header "IntD";
    Length None;
    Integer32 None; (* operating system id *)
    Bit None; (* should copy *)
    Bit None; (* is OS dependent *)
    Integer8 None; (* contents id *)
    Integer16 None; (* reserved *)
    Integer32 None; (* interpreter id *)
    RemainingBytes None (* data *)];;
*)

let ifzs_ifhd =
  Record [
    Header "IFhd";
    Length None;
    Integer16 None; (* release number *)
    ByteString (None, 6); (* serial number *)
    Integer16 None; (* checksum *)
    Integer24 None];; (* program counter *)

let ifzs_frame =
  Record [
    Integer24 None; (* return address *)
    BitField [
      Assign ("v", Integer4 None);  (* count of local variables *)
      Bit (4, None)]; (* caller discards result *)
    Integer8 None; (* variable caller will store result in *)
    BitField [
      Bit (0, None); (* argument 0 was supplied *)
      Bit (1, None); (* argument 1 was supplied *)
      Bit (2, None); (* argument 2 was supplied *)
      Bit (3, None); (* argument 3 was supplied *)
      Bit (4, None); (* argument 4 was supplied *)
      Bit (5, None); (* argument 5 was supplied *)
      Bit (6, None)]; (* argument 6 was supplied *)
    Assign ("n", Integer16 None); (* size of evaluation stack in words *)
    SizedList (Lookup "v", [Integer16 None]); (* local variables *)
    SizedList (Lookup "n", [Integer16 None])];; (* evaluation stack *)

let ifzs_stacks =
  Record [
    Header "Stks";
    Length None;
    UnsizedList [ifzs_frame]];;

let ifzs_cmem =
  Record [
    Header "CMem";
    Length None;
    RemainingBytes None];;

let ifzs_umem =
  Record [
    Header "UMem";
    Length None;
    RemainingBytes None];;

let ifzd_form =
  Record [
    Header "FORM";
    Length None;
    SubHeader "IFZS";
    UnorderedList [
      ifzs_ifhd;
      ifzs_stacks;
      ifzs_umem;
      ifzs_cmem]];;



let display_bytes text =
    let blocksize = 16 in
    let length = String.length text in
    let rec print_loop i acc =
        if i = length then
            acc
        else (
            let s =
                if i mod blocksize = 0 then
                    Printf.sprintf "\n%06x: " i
                else
                    "" in
            let c = text.[i] in
            let ci = int_of_char c in
            let cd = if 32 <= ci && ci <= 126 then c else ' ' in
            let s2 = Printf.sprintf "%02x|%c " ci cd in
        print_loop (i + 1) (acc ^ s ^ s2)) in
    (print_loop 0 "") ^ "\n";;





let filename = "ZORK1.sav";;
let channel = open_in_bin filename in
let length = in_channel_length channel in
let text = really_input_string channel length in
Printf.printf "%s\n" (display_bytes text);
close_in channel;;

let r = read_iff_file "ZORK1.sav" ifzd_form;;
