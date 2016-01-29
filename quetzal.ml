open Iff
open Type

let ifzs_ifhd =
  Record [
    Header "IFhd";
    Length None;
    Integer16 None;       (* release number *)
    ByteString (None, 6); (* serial number *)
    Integer16 None;       (* checksum *)
    Integer24 None]       (* program counter *)

let ifzs_frame =
  Record [
    Integer24 None; (* return address *)
    BitField [
      Assign ("v", Integer4 None);  (* count of local variables *)
      Bit (4, None)]; (* caller discards result *)
    Integer8 None;    (* variable caller will store result in *)
    BitField [
      Bit (0, None);  (* argument 0 was supplied *)
      Bit (1, None);  (* argument 1 was supplied *)
      Bit (2, None);  (* argument 2 was supplied *)
      Bit (3, None);  (* argument 3 was supplied *)
      Bit (4, None);  (* argument 4 was supplied *)
      Bit (5, None);  (* argument 5 was supplied *)
      Bit (6, None)]; (* argument 6 was supplied *)
    Assign ("n", Integer16 None); (* size of evaluation stack in words *)
    SizedList (Lookup "v", [Integer16 None]); (* local variables *)
    SizedList (Lookup "n", [Integer16 None])] (* evaluation stack *)

let ifzs_stacks =
  Record [
    Header "Stks";
    Length None;
    UnsizedList [ifzs_frame]]

let ifzs_cmem =
  Record [
    Header "CMem";
    Length None;
    RemainingBytes None]

let ifzs_umem =
  Record [
    Header "UMem";
    Length None;
    RemainingBytes None]

let ifzd_form =
  Record [
    Header "FORM";
    Length None;
    SubHeader "IFZS";
    UnorderedList [
      ifzs_ifhd;
      ifzs_stacks;
      ifzs_umem;
      ifzs_cmem]]

let save
  (Release_number release)
  (Serial_number serial)
  (Checksum checksum)
  (Instruction pc)
  (Compressed compressed) frames =
  Record [
    Header "FORM";
    Length None; (* The writer will figure it out *)
    SubHeader "IFZS";
    UnorderedList [
      Record [
        Header "IFhd";
        Length None;
        Integer16 (Some release);
        ByteString (Some serial, 6);
        Integer16 (Some checksum);
        Integer24 (Some pc) ];
      Record [
        Header "CMem";
        Length None;
        RemainingBytes (Some compressed)];
      Record [
        Header "Stks";
        Length None;
        UnsizedList frames] ] ]

let read_ifzd_chunks ifzd =
  match ifzd with
  | Record [
      Header "FORM";
      Length _;
      SubHeader "IFZS";
      UnorderedList items] ->
      items
  | _ -> failwith "TODO: Handle failure reading ifzd"

let read_header ifzd =
  let chunks = read_ifzd_chunks ifzd in
  let ifhd = find_record chunks "IFhd" in
  match ifhd with
  | Some Record [
      Header "IFhd";
      Length _;
      Integer16 Some release_number;
      ByteString (Some serial_number, 6);
      Integer16 Some checksum;
      Integer24 Some pc ] ->
    ((Release_number release_number), (Serial_number serial_number), (Checksum checksum), pc)
  | _ -> failwith "TODO handle failure reading ifhd"

let read_stacks ifzd =
  let chunks = read_ifzd_chunks ifzd in
  let stacks_chunk = find_record chunks "Stks" in
  match stacks_chunk with
  | Some Record [
      Header "Stks";
      Length _;
      UnsizedList items ] -> items
  | _ -> failwith "TODO handle failure reading stacks"

let read_memory ifzd =
  let chunks = read_ifzd_chunks ifzd in
  let cmem_chunk = find_record chunks "CMem" in
  let umem_chunk = find_record chunks "UMem" in
  let compressed = match cmem_chunk with
    | None -> None
    | Some Record [
        Header "CMem";
        Length Some length;
        RemainingBytes Some bytes] ->
      Some (Compressed bytes)
    | _ -> failwith "TODO: Handle failure reading CMem" in
  let uncompressed = match umem_chunk with
    | None -> None
    | Some Record [
        Header "UMem";
        Length Some length;
        RemainingBytes Some bytes] ->
      Some (Uncompressed bytes)
    | _ -> failwith "TODO: Handle failure reading UMem" in
  (compressed, uncompressed)
