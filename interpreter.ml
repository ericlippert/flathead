open Utility
open Type

type t =
{
  story : Story.t;
  program_counter : instruction_address;
  frames : Frameset.t;
}

let make story =
  {
    story = story;
    program_counter = Story.initial_program_counter story;
    frames = Frameset.make Frame.empty;
}

let current_frame interpreter =
  Frameset.current_frame interpreter.frames

let add_frame interpreter frame =
  { interpreter with frames = Frameset.add_frame interpreter.frames frame }

let remove_frame interpreter =
  { interpreter with frames = Frameset.remove_frame interpreter.frames }

let peek_stack interpreter =
  Frameset.peek_stack interpreter.frames

let pop_stack interpreter =
  { interpreter with frames = Frameset.pop_stack interpreter.frames }

let push_stack interpreter value =
  { interpreter with frames = Frameset.push_stack interpreter.frames value }

let program_counter interpreter =
  interpreter.program_counter

let set_program_counter interpreter new_program_counter =
  { interpreter with program_counter = new_program_counter }

let read_local interpreter local =
  Frameset.read_local interpreter.frames local

let write_local interpreter local value =
  { interpreter with frames = Frameset.write_local interpreter.frames local value }

let read_global interpreter global =
  Globals.read interpreter.story global

let write_global interpreter global value =
  { interpreter with story = Globals.write interpreter.story global value }

let read_variable interpreter variable =
  match variable with
  | Stack -> (peek_stack interpreter, pop_stack interpreter)
  | Local_variable local -> (read_local interpreter local, interpreter)
  | Global_variable global -> (read_global interpreter global, interpreter)

let write_variable interpreter variable value =
  match variable with
  | Stack -> push_stack interpreter value
  | Local_variable local -> write_local interpreter local value
  | Global_variable global -> write_global interpreter global value

let read_operand interpreter operand =
  match operand with
  | Large large -> (large, interpreter)
  | Small small -> (small, interpreter)
  | Variable v -> read_variable interpreter v

(* Takes a list of operands, produces a list of arguments. *)
let operands_to_arguments interpreter operands =
  let rec aux (args, interp) ops =
    match ops with
    | [] -> (args, interp)
    | h :: t ->
      let (argument, new_interpreter) = read_operand interp h in
      aux ((argument :: args), new_interpreter) t in
  let (args_rev, final_interpreter) = aux ([], interpreter) operands in
  ((List.rev args_rev), final_interpreter)

let interpret_store interpreter store result =
  match store with
  | None -> interpreter
  | Some variable -> write_variable interpreter variable result

let display_current_instruction interpreter =
  let address = interpreter.program_counter in
  let instruction = Instruction.decode interpreter.story address in
  Instruction.display instruction interpreter.story

(* Debugging method *)
let display interpreter =
  let frames = Frameset.display interpreter.frames in
  let instr = display_current_instruction interpreter in
  Printf.sprintf "\n---\n%s\n%s\n" frames instr


(* This routine handles all call instructions:

2OP:25  call_2s  routine arg -> (result)
2OP:26  call_2n  routine arg
1OP:136 call_1s  routine -> (result)
1OP:143 call_1n  routine
VAR:224 call_vs  routine up-to-3-arguments -> (result)
VAR:236 call_vs2 routine up-to-7-arguments -> (result)
VAR:249 call_vn  routine up-to-3-arguments
VAR:250 call_vn2 routine up-to-7-arguments

The "s" versions store the result; the "n" versions discard it. *)

let handle_call routine_address arguments interpreter instruction =
  if routine_address = 0 then
    (* Spec: When the address 0 is called as a routine, nothing happens and the
     return value is false. *)
    let result = 0 in
    let store = Instruction.store instruction in
    let store_interpreter = interpret_store interpreter store result in
    let addr = Instruction.following instruction in
    set_program_counter store_interpreter addr
  else
    let routine_address = Packed_routine routine_address in
    let routine_address = Story.decode_routine_packed_address interpreter.story routine_address in
    let resume_at = Instruction.following instruction in
    let store = Instruction.store instruction in
    let frame = Frame.make interpreter.story arguments routine_address resume_at store in
    let pc = Routine.first_instruction interpreter.story routine_address in
    set_program_counter (add_frame interpreter frame) pc

(* Move the interpreter on to the next instruction *)
let step_instruction interpreter =
  let instruction = Instruction.decode interpreter.story interpreter.program_counter in
  let operands = Instruction.operands instruction in
  let (arguments, interpreter) = operands_to_arguments interpreter operands in
  let opcode = Instruction.opcode instruction in
  match (opcode, arguments) with
  | (VAR_224, routine :: args) -> handle_call routine args interpreter instruction
  | _ -> failwith (Printf.sprintf "TODO: %s " (Instruction.display instruction interpreter.story))
  (* End step_instruction *)
