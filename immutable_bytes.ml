open Type
open Utility

module IntMap = Map.Make(struct type t = int let compare = compare end)

(* TODO: Consider: is it worthwhile to make a tree of int32s or int64s
 instead of chars? The total memory consumed by all the nodes
 would be smaller. *)

type t =
{
  original_bytes : string;
  edits : char IntMap.t
}

let make bytes =
  { original_bytes = bytes; edits = IntMap.empty }

let size bytes =
  String.length bytes.original_bytes

let read_byte bytes (Byte_address address) =
  if address < 0 then
    failwith "address is negative"
  else if address >= (size bytes) then
    failwith "address is out of range"
  else
    let c =
      if IntMap.mem address bytes.edits then IntMap.find address bytes.edits
      else bytes.original_bytes.[address] in
    int_of_char c

let write_byte bytes (Byte_address address) value =
  if address < 0 then
    failwith "address is negative"
  else if address >= (size bytes) then
    failwith "address is out of range"
  else
    let b = char_of_int (byte_of_int value) in
    { bytes with edits = IntMap.add address b bytes.edits }

let original bytes =
  { bytes with edits = IntMap.empty }
