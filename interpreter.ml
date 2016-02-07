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

let set_program_counter interpreter program_counter =
  { interpreter with program_counter }

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
  
let interpret_return interpreter value =
 let frame = current_frame interpreter in
 let next_pc = Frame.resume_at frame in
 let store = Frame.store frame in
 let pop_frame_interpreter = remove_frame interpreter in
 let result_interpreter = set_program_counter pop_frame_interpreter next_pc in
 interpret_store result_interpreter store value

let interpret_branch interpreter instruction result =
  let result = not (result = 0) in 
  let following = Instruction.following instruction in
  match Instruction.branch instruction with
  | None -> set_program_counter interpreter following 
  | Some (sense, Return_false) ->
    if result = sense then interpret_return interpreter 0
    else set_program_counter interpreter following 
  | Some (sense, Return_true) ->
    if result = sense then interpret_return interpreter 1
    else set_program_counter interpreter following 
  | Some (sense, Branch_address branch_target) ->
    if result = sense then set_program_counter interpreter branch_target
    else set_program_counter interpreter following 
    
let interpret_instruction interpreter instruction handler =
  let (result, handler_interpreter) = handler interpreter in
  let store = Instruction.store instruction in
  let store_interpreter = interpret_store handler_interpreter store result in
  interpret_branch store_interpreter instruction result

let interpret_value_instruction interpreter instruction handler =
  let result = handler interpreter in
  let store = Instruction.store instruction in
  let store_interpreter = interpret_store interpreter store result in
  interpret_branch store_interpreter instruction result

let interpret_effect_instruction interpreter instruction handler =
  let handler_interpreter = handler interpreter in
  let result = 0 in
  let store = Instruction.store instruction in
  let store_interpreter = interpret_store handler_interpreter store result in
  interpret_branch store_interpreter instruction result

let display_current_instruction interpreter =
  let address = interpreter.program_counter in
  let instruction = Instruction.decode interpreter.story address in
  Instruction.display instruction interpreter.story

(* Debugging method *)
let display interpreter =
  let frames = Frameset.display interpreter.frames in
  let instr = display_current_instruction interpreter in
  Printf.sprintf "\n---\n%s\n%s\n" frames instr
  
(* Spec: 2OP:1 je a b ?label
Jump if a is equal to any of the subsequent operands. (Thus @je a never
jumps and @je a b jumps if a = b.) *)

(* Note: Already we are off to a bad start; the revision to the spec says:

je can take between 2 and 4 operands. je with just 1 operand is not permitted.

Note that je is one of the rare "2OP" instructions that can take 3 or 4
operands. *)

let handle_je2 a b interpreter =
  if a = b then 1 else 0

let handle_je3 a b c interpreter =
  if a = b || a = c then 1 else 0

let handle_je4 a b c d interpreter =
  if a = b || a = c || a = d then 1 else 0

(* Spec: 2OP:2 jl a b ?(label)
  Jump if a < b  using a signed 16-bit comparison. *)

let handle_jl a b interpreter =
  let a = signed_word a in
  let b = signed_word b in
  if a < b then 1 else 0

(* Spec: 2OP:3 3 jg a b ?(label)
  Jump if a > b using a signed 16-bit comparison. *)

let handle_jg a b interpreter =
  let a = signed_word a in
  let b = signed_word b in
  if a > b then 1 else 0

(* Spec: 2OP:20 add a b -> (result)
  Signed 16-bit addition. *)

let handle_add a b interpreter =
  a + b

(* Spec: 2OP:21 sub a b -> (result)
  Signed 16-bit subtraction. *)

let handle_sub a b interpreter =
  a - b

(* Spec: 2OP:22 mul a b -> (result)
  Signed 16-bit multiplication. *)

let handle_mul a b interpreter =
  a * b

(* Spec: 2OP:23 div a b -> (result)
  Signed 16-bit division.  Division by zero should halt
  the interpreter with a suitable error message. *)

let handle_div a b interpreter =
  let a = signed_word a in
  let b = signed_word b in
  a / b

(* Spec: 2OP:24 mod a b -> (result)
  Remainder after signed 16-bit division. Division by zero should halt
  the interpreter with a suitable error message. *)

let handle_mod a b interpreter =
  let a = signed_word a in
  let b = signed_word b in
  a mod b

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
    
(* Spec: 1OP:128 jz a ?(label)
  Jump if a = 0. *)

let handle_jz a interpreter =
  if a = 0 then 1 else 0

(* Spec: 1OP:139 ret value
  Returns from the current routine with the value given *)

let handle_ret result interpreter =
    interpret_return interpreter result

(* Move the interpreter on to the next instruction *)
let step_instruction interpreter =
  let instruction = Instruction.decode interpreter.story interpreter.program_counter in
  let operands = Instruction.operands instruction in
  let (arguments, interpreter) = operands_to_arguments interpreter operands in
  let interpret_instruction = interpret_instruction interpreter instruction in
  let value = interpret_value_instruction interpreter instruction in
  let effect = interpret_effect_instruction interpreter instruction in
  let opcode = Instruction.opcode instruction in
  match (opcode, arguments) with
  | (OP2_1, [a; b]) -> value (handle_je2 a b)
  | (OP2_1, [a; b; c]) -> value (handle_je3 a b c)
  | (OP2_1, [a; b; c; d]) -> value (handle_je4 a b c d)
  | (OP2_2, [a; b]) -> value (handle_jl a b)
  | (OP2_3, [a; b]) -> value (handle_jg a b)
  | (OP2_20, [a; b]) -> value (handle_add a b)
  | (OP2_21, [a; b]) -> value (handle_sub a b)
  | (OP2_22, [a; b]) -> value (handle_mul a b)
  | (OP2_23, [a; b]) -> value (handle_div a b)
  | (OP2_24, [a; b]) -> value (handle_mod a b)
  | (OP1_128, [a]) -> value (handle_jz a)
  | (OP1_139, [result]) -> handle_ret result interpreter 
  | (VAR_224, routine :: args) -> handle_call routine args interpreter instruction
  | _ -> failwith (Printf.sprintf "TODO: %s " (Instruction.display instruction interpreter.story))
  (* End step_instruction *)
