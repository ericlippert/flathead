open Type
open Utility

let () = 
  let version_address = Byte_address 0 in
  let story = Story.load "minizork.z3" in
  let version = Story.read_byte story version_address in
  Printf.printf "%d\n" version

