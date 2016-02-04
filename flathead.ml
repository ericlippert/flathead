open Type
open Utility

let () = 
  let story = Story.load "minizork.z3" in
  let interpreter1 = Interpreter.make story in
  let interpreter2 = Interpreter.step_instruction interpreter1 in
  let interpreter3 = Interpreter.step_instruction interpreter2 in
  let text1 = Interpreter.display interpreter1 in
  let text2 = Interpreter.display interpreter2 in
  let text3 = Interpreter.display interpreter3 in
  Printf.printf "%s\n%s\n%s\n" text1 text2 text3
