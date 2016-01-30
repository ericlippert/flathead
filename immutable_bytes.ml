open Type
open Utility

module IntMap = Map.Make(struct type t = int let compare = compare end)

type t =
{
  original_bytes : string;
  edits : char IntMap.t
}

let make bytes =
  { original_bytes = bytes; edits = IntMap.empty }

let size bytes =
  String.length bytes.original_bytes

let read_byte bytes address =
  if is_out_of_range address (size bytes) then
    failwith "address is out of range"
  else
    let (Byte_address addr) = address in
    let c =
      if IntMap.mem addr bytes.edits then IntMap.find addr bytes.edits
      else bytes.original_bytes.[addr] in
    int_of_char c

let write_byte bytes address value =
  if is_out_of_range address (size bytes) then
    failwith "address is out of range"
  else
    let (Byte_address addr) = address in
    let b = char_of_int (byte_of_int value) in
    { bytes with edits = IntMap.add addr b bytes.edits }

let original bytes =
  { bytes with edits = IntMap.empty }
