open Type
open Utility

let () = 
  let story = Story.load "minizork.z3" in
  let text = Reachability.display_reachable_instructions story (Instruction 0x37d9) in
  Printf.printf "%s\n" text
