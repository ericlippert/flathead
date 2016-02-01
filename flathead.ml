open Type
open Utility

let () = 
  let story = Story.load "minizork.z3" in
  let instruction = Instruction.decode story (Instruction 0x37d9) in
  let text = Instruction.display instruction (Story.version story) in
  Printf.printf "%s\n" text
