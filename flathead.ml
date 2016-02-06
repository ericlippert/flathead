open Type
open Utility

let rec interpreter_loop interpreter =
  Printf.printf "%s" (Interpreter.display_current_instruction interpreter);
  flush stdout;
  interpreter_loop (Interpreter.step_instruction interpreter)


let () = 
  let story = Story.load "minizork.z3" in
  interpreter_loop (Interpreter.make story)
