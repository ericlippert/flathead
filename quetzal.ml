open Iff

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
