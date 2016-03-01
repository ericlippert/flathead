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

let read_variable_in_place interpreter variable =
  match variable with
  | Stack -> peek_stack interpreter
  | Local_variable local -> read_local interpreter local
  | Global_variable global -> read_global interpreter global

let write_variable interpreter variable value =
  match variable with
  | Stack -> push_stack interpreter value
  | Local_variable local -> write_local interpreter local value
  | Global_variable global -> write_global interpreter global value

let write_variable_in_place interpreter variable value =
  match variable with
  | Stack -> push_stack (pop_stack interpreter) value
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
  
let print interpreter text =
  Printf.printf "%s" text;
  flush stdout;
  interpreter
  
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
  
(* Spec: 2OP:4 dec_chk (variable) value ?(label)
  Decrement variable, and branch if it is now less than the given value.

This one is odd. The value determined for the first argument is treated as
a variable, which is then decremented. So if the first argument is "sp"
then the stack is popped; if the value taken off the stack was, say, 50,
then global 50 is decremented. *)

let handle_dec_chk variable value interpreter =
  let variable = Instruction.decode_variable variable in
  let value = signed_word value in
  let original = read_variable_in_place interpreter variable in
  let original = signed_word original in
  let decremented = signed_word (original - 1) in
  let write_interpreter = write_variable_in_place interpreter variable decremented in
  let result = if decremented < value then 1 else 0 in
  (result, write_interpreter)

(* Spec: 2OP:5 inc_chk (variable) value ?(label)
  Increment variable, and branch if now greater than value. *)

let handle_inc_chk variable value interpreter =
  let variable = Instruction.decode_variable variable in
  let value = signed_word value in
  let original = read_variable_in_place interpreter variable in
  let original = signed_word original in
  let incremented = signed_word (original + 1) in
  let write_interpreter = write_variable_in_place interpreter variable incremented in
  let result = if incremented > value then 1 else 0 in
  (result, write_interpreter)
  
(* Spec: 2OP:6 jin obj1 obj2 ?(label)
  Jump if obj1 is a direct child of obj2, i.e., if parent of obj1 is obj2. *)

(* TODO: The spec is unclear as to what happens if obj1 is an invalid object
number, such as 0. On the one hand, that's not even an object, so asking
if this object is a child of another is an invalid question. On the other
hand, an invalid object is not a direct child of any object.*)

let handle_jin obj1 obj2 interpreter =
  let obj1 = Object obj1 in
  let obj2 = Object obj2 in
  let parent = Object.parent interpreter.story obj1 in
  if parent = obj2 then 1 else 0
  
(* Spec: 2OP:7 test bitmap flags ?(label)
  Jump if all of the flags in bitmap are set (i.e. if bitmap & flags == flags) *)

let handle_test bitmap flags interpreter =
  if (bitmap land flags) = flags then 1 else 0

(* Spec: 2OP:8 8 or a b -> (result)
  Bitwise OR. *)

let handle_or a b interpreter =
  a lor b

(* Spec: 2OP:9 and a b -> (result)
Bitwise AND. *)

let handle_and a b interpreter =
  a land b
  
(* Spec: 2OP:10 test_attr object attribute ?(label)
  Jump if object has attribute. *)

let handle_test_attr obj attr interpreter =
  let obj = Object obj in
  let attr = Attribute attr in
  if Object.attribute interpreter.story obj attr then 1 else 0

(* Spec:  2OP:11 set_attr object attribute
  Make object have the attribute numbered attribute. *)

let handle_set_attr obj attr interpreter =
  let obj = Object obj in
  let attr = Attribute attr in
  { interpreter with story = Object.set_attribute interpreter.story obj attr }

(* Spec: 2OP:12 clear_attr object attribute
  Make object not have the attribute numbered attribute. *)

let handle_clear_attr obj attr interpreter =
  let obj = Object obj in
  let attr = Attribute attr in
  { interpreter with story = Object.clear_attribute interpreter.story obj attr }
  
(* Spec: 2OP:13 store (variable) value
  Set the variable referenced by the operand to value *)

(* This is one of those odd instructions like dec_chk (described above)
that takes a variable number as an operand. *)

let handle_store variable value interpreter =
  let variable = Instruction.decode_variable variable in
  write_variable_in_place interpreter variable value
  
(* Spec: 2OP:14 insert_obj object destination
  Moves object O to become the first child of the destination object D.
  (Thus, after the operation the child of D is O, and the sibling of O
  is whatever was previously the child of D.) All children of O move with it.
  (Initially O can be at any point in the object tree; it may legally
  have parent zero.) *)

let handle_insert_obj obj destination interpreter =
  let obj = Object obj in
  let destination = Object destination in
  { interpreter with story = Object.insert interpreter.story obj destination }

(* Spec: 2OP:15 loadw array word-index -> (result)
Stores array-->word-index (i.e., the word at address array+2*word-index,
which must lie in static or dynamic memory). *)

let handle_loadw arr idx interpreter =
  let arr = Word_address arr in
  Story.read_word interpreter.story (inc_word_addr_by arr idx)

(* Spec: 2OP:16 loadb array byte-index -> (result)
Stores array->byte-index (i.e., the byte at address array+byte-index,
which must lie in static or dynamic memory). *)

let handle_loadb arr idx interpreter =
  let arr = Byte_address arr in
  Story.read_byte interpreter.story (inc_byte_addr_by arr idx)

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
  
(* Spec: 1OP:129 get_sibling object -> (result) ?(label)
  Get next object in tree, branching if this exists, i.e. is not 0 *)

let handle_get_sibling obj interpreter =
  let obj = Object obj in
  let (Object sibling) = Object.sibling interpreter.story obj in
  sibling

(* Spec: 1OP:130 get_child object -> (result) ?(label)
  Get first object contained in given object, branching if this exists,
  i.e., is not 0 *)

let handle_get_child obj interpreter =
  let obj = Object obj in
  let (Object child) = Object.child interpreter.story obj in
  child

(* Spec: 1OP:131 get_parent object -> (result)
  Get parent object (note that this has no "branch if exists" clause). *)

let handle_get_parent obj interpreter =
  let obj = Object obj in
  let (Object parent) = Object.parent interpreter.story obj in
  parent
  
(* Spec: 1OP:133 inc (variable)
  Increment variable by 1. (This is signed, so -1 increments to 0.)

Note that this is another of those unusual instructions that takes as an
argument the number of a variable. *)

let handle_inc variable interpreter =
  let variable = Instruction.decode_variable variable in
  let original = read_variable_in_place interpreter variable in
  let incremented = original + 1 in
  write_variable_in_place interpreter variable incremented

(* Spec: 1OP:134 dec (variable)
  Decrement variable by 1. This is signed, so 0 decrements to -1.

  Note that this is another of those unusual instructions that takes as an
  argument the number of a variable. *)

let handle_dec variable interpreter =
  let variable = Instruction.decode_variable variable in
  let original = read_variable_in_place interpreter variable in
  let decremented = original - 1 in
  write_variable_in_place interpreter variable decremented
  
(* Spec: 1OP:135 print_addr byte-address-of-string
  Print (Z-encoded) string at given byte address, in dynamic or static memory *)

let handle_print_addr addr interpreter =
  let addr = Zstring addr in
  let text = Zstring.read interpreter.story addr in
  print interpreter text

(* Spec: 1OP:137 remove_obj object
Detach the object from its parent, so that it no longer has any parent.
(Its children remain in its possession.) *)
let handle_remove_obj obj interpreter =
  let obj = Object obj in
  { interpreter with story = Object.remove interpreter.story obj}
  
(* Spec: 1OP:138 print_obj object
  Print short name of object (the Z-encoded string in the object header,
  not a property). If the object number is invalid, the interpreter should
  halt with a suitable error message. *)

let handle_print_obj obj interpreter =
  let obj = Object obj in
  let text = Object.name interpreter.story obj in
  print interpreter text

(* Spec: 1OP:139 ret value
  Returns from the current routine with the value given *)

let handle_ret result interpreter =
    interpret_return interpreter result
    
(* Spec: 1OP:140 jump ?(label)
Jump (unconditionally) to the given label. (This is not a branch instruction
and the operand is a 2-byte signed offset to apply to the program counter.)
It is legal for this to jump into a different routine (which should not
change the routine call state), although it is considered bad practice to
do so and the Txd disassembler is confused by it.

Note: the revised specification clarifies:

The destination of the jump opcode is
Address after instruction + Offset - 2
This is analogous to the calculation for branch offsets. *)

let handle_jump offset interpreter instruction =
  let offset = signed_word offset in
  let target = Instruction.jump_address instruction offset in
  set_program_counter interpreter target
  
(* Spec: 1OP:141 print_paddr packed-address-of-string
  Print the (Z-encoded) string at the given packed address in high memory. *)

let handle_print_paddr packed_address interpreter =
  let packed_address = Packed_zstring packed_address in
  let address = Story.decode_string_packed_address interpreter.story packed_address in
  let text = Zstring.read interpreter.story address in
  print interpreter text

(* Spec: 1OP:142 load (variable) -> (result)
  The value of the variable referred to by the operand is stored in the result. *)

let handle_load variable interpreter =
  (* The value of the operand is a number which represents the variable
  to be read from. So, for example, if the operand is sp then the
  value of the operand is the top of the stack, and the top of the stack
  contains a number. That number is then interpreted as a variable, and
  the value read from *that* variable is the result of the load. 
  
  However, if the value of the operand is zero then the value stored
  is the value on top of the stack, but the stack is not popped. *)
  
  let variable = Instruction.decode_variable variable in
  read_variable_in_place interpreter variable
  
(* Spec: 0OP:176 rtrue
  Return true (i.e., 1) from the current routine. *)

let handle_rtrue interpreter instruction =
  interpret_return interpreter 1

(* Spec: 0OP:177 rfalse
  Return false (i.e., 0) from the current routine. *)

let handle_rfalse interpreter instruction =
  interpret_return interpreter 0
  
(* Spec: 0OP:178 print
  Print the quoted (literal) Z-encoded string. *)

let handle_print interpreter instruction =
  let printed_interpreter = match Instruction.text instruction with
  | Some text -> print interpreter text
  | None -> interpreter in
  interpret_branch printed_interpreter instruction 0

(* Spec: 0OP:179 print_ret
  Print the quoted (literal) Z-encoded string, then print a new-line
  and then return true (i.e., 1) *)

let handle_print_ret interpreter instruction =
  let printed_interpreter =
    match Instruction.text instruction with
    | Some text -> print interpreter (text ^ "\n")
    | None -> interpreter in
  interpret_return printed_interpreter 1
  
(* Spec:0OP:184 ret_popped
  Pops top of stack and returns that. (This is equivalent to ret sp, but
  is one byte cheaper.) *)

let handle_ret_popped interpreter instruction =
  let result = peek_stack interpreter in
  let popped_interpreter = pop_stack interpreter in
  interpret_return popped_interpreter result

(* 0OP:185 pop
  Throws away the top item on the stack. (This was useful to lose unwanted
  routine call results in early Versions.) *)

let handle_pop interpreter =
  pop_stack interpreter
  
(* Spec: 0OP:185 catch -> (result)
  Opposite to throw (and occupying the same opcode that pop used in
  Versions 3 and 4). catch returns the current "stack frame". *)

let handle_catch interpreter =
  failwith "TODO: catch instruction not yet implemented"
  
(* Spec: 0OP:187 new_line
  Print carriage return. *)

let handle_new_line interpreter =
  print interpreter "\n"

(* VAR:225 storew array wordindex value
  array->wordindex = value
  i.e. stores the given value in the word at address array + 2 * wordindex
  (which must lie in dynamic memory).  *)

let handle_storew arr ind value interpreter =
  let arr = Word_address arr in
  let addr = inc_word_addr_by arr ind in
  { interpreter with story = Story.write_word interpreter.story addr value }

(* Spec: VAR:226 storeb array byteindex value
  array->byteindex = value, i.e. stores the given value in the byte at
  address array+byteindex (which must lie in dynamic memory). *)
  
let handle_storeb arr ind value interpreter =
  let arr = Byte_address arr in
  let addr = inc_byte_addr_by arr ind  in
  { interpreter with story = Story.write_byte interpreter.story addr value }
  
(* Spec: VAR:229 print_char output-character-code
  Print a ZSCII character. The operand must be a character code defined in
  ZSCII for output (see Section 3). In particular, it must certainly not be
  negative or larger than 1023. *)

let handle_print_char code interpreter =
  let text = string_of_char (char_of_int code) in
  print interpreter text

(* Spec: VAR:230 print_num value
  Print (signed) number in decimal. *)

let handle_print_num value interpreter =
  let value = signed_word value in
  let text = Printf.sprintf "%d" value in
  print interpreter text

(* Spec: VAR:233 pull (variable)
                 pull stack -> (result)
Pulls value off a stack. (If the stack underflows, the interpreter should halt with a suitable error
message.) In Version 6, the stack in question may be specified as a user one: otherwise it is the
game stack.
push*)

let handle_pull1 x interpreter =
  if (Story.version interpreter.story) = V6 then
    failwith "TODO: user stack pull not yet implemented"
  else
    (* In non-v6, this is another one of those odd instructions
    whose operand identifies a variable. *)
    let variable = Instruction.decode_variable x in
    let value = peek_stack interpreter in
    let popped_interpreter = pop_stack interpreter in
    let store_interpreter = write_variable_in_place popped_interpreter variable value in
    (0, store_interpreter)

let handle_pull0 interpreter =
  (* In version 6 if the operand is omitted then we simply pop the stack
  and store the result normally. *)
  let result = peek_stack interpreter in
  let popped_interpreter = pop_stack interpreter in
  (result, popped_interpreter)

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
  | (OP2_4, [variable; value]) -> interpret_instruction (handle_dec_chk variable value)
  | (OP2_5, [variable; value]) -> interpret_instruction (handle_inc_chk variable value)
  | (OP2_6, [obj1; obj2]) -> value (handle_jin obj1 obj2)
  | (OP2_7, [bitmap; flags]) -> value (handle_test bitmap flags)
  | (OP2_8, [a; b]) -> value (handle_or a b)
  | (OP2_9, [a; b]) -> value (handle_and a b)
  | (OP2_10, [obj; attr]) -> value (handle_test_attr obj attr)
  | (OP2_11, [obj; attr]) -> effect (handle_set_attr obj attr)
  | (OP2_12, [obj; attr]) -> effect (handle_clear_attr obj attr)
  | (OP2_13, [variable; value]) -> effect (handle_store variable value)
  | (OP2_14, [obj; destination]) -> effect (handle_insert_obj obj destination)
  | (OP2_15, [arr; idx]) -> value (handle_loadw arr idx)
  | (OP2_16, [arr; idx]) -> value (handle_loadb arr idx)
  | (OP2_20, [a; b]) -> value (handle_add a b)
  | (OP2_21, [a; b]) -> value (handle_sub a b)
  | (OP2_22, [a; b]) -> value (handle_mul a b)
  | (OP2_23, [a; b]) -> value (handle_div a b)
  | (OP2_24, [a; b]) -> value (handle_mod a b)
  | (OP1_128, [a]) -> value (handle_jz a)
  | (OP1_129, [obj]) -> value (handle_get_sibling obj)
  | (OP1_130, [obj]) -> value (handle_get_child obj)
  | (OP1_131, [obj]) -> value (handle_get_parent obj)
  | (OP1_133, [variable]) -> effect (handle_inc variable)
  | (OP1_134, [variable]) -> effect (handle_dec variable)
  | (OP1_135, [address]) -> effect (handle_print_addr address)
  | (OP1_137, [obj]) -> effect (handle_remove_obj obj)
  | (OP1_138, [obj]) -> effect (handle_print_obj obj)
  | (OP1_139, [result]) -> handle_ret result interpreter 
  | (OP1_140, [offset]) -> handle_jump offset interpreter instruction
  | (OP1_141, [paddr]) -> effect (handle_print_paddr paddr)
  | (OP1_142, [variable]) -> value (handle_load variable)
  | (OP0_176, []) -> handle_rtrue interpreter instruction
  | (OP0_177, []) -> handle_rfalse interpreter instruction
  | (OP0_178, []) -> handle_print interpreter instruction
  | (OP0_179, []) -> handle_print_ret interpreter instruction
  | (OP0_184, []) -> handle_ret_popped interpreter instruction
  | (OP0_185, []) ->
    if Story.v4_or_lower (Story.version interpreter.story) then effect handle_pop
    else value handle_catch
  | (OP0_187, []) -> effect handle_new_line
  | (VAR_224, routine :: args) -> handle_call routine args interpreter instruction
  | (VAR_225, [arr; ind; value]) -> effect (handle_storew arr ind value)
  | (VAR_226, [arr; ind; value]) -> effect (handle_storeb arr ind value)
  | (VAR_229, [code]) -> effect (handle_print_char code)
  | (VAR_230, [number]) -> effect (handle_print_num number)
  | (VAR_233, []) -> interpret_instruction handle_pull0
  | (VAR_233, [x]) -> interpret_instruction (handle_pull1 x)
  | _ -> failwith (Printf.sprintf "TODO: %s " (Instruction.display instruction interpreter.story))
  (* End step_instruction *)
