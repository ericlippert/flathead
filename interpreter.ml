open Utility
open Iff
open Quetzal
open Type

type interpreter_state =
  | Running
  | Waiting_for_input
  | Halted

type output_stream_kind =
  | ScreenStream
  | TranscriptStream
  | MemoryStream
  | CommandStream

(* The state of the interpreter *)
type t =
{
  story : Story.t;
  program_counter : instruction_address;
  frames : Frameset.t;
  random : Randomness.t;
  state : interpreter_state;

  (* output stream 1 *)
  screen : Screen.t;
  has_new_output : bool;
  screen_selected : bool;

  (* output stream 2 *)
  transcript : Transcript.t;
  transcript_selected : transcript_enabled;

  memory_table : word_prefixed list;
  memory_selected : bool;

  (* output stream 4 *)
  commands : string list;
  commands_selected : bool;

  (* TODO: Other input streams *)

  (* TODO: What are the types of these addresses? *)
  text_address : input_buffer;
  parse_address : parse_buffer;
  input : string;
  input_max : int;
}

let make story screen =
  (* TODO: Restore these after a restart / restore *)

  let story = Story.set_colours_supported story (Colours_supported false) in
  let story = Story.set_pictures_supported story (Pictures_supported false) in
  let story = Story.set_boldface_supported story (Boldface_supported false) in
  let story = Story.set_italics_supported story (Italics_supported false) in
  let story = Story.set_status_line_supported story (Status_line_supported true) in
  let story = Story.set_fixed_pitch_supported story (Fixed_pitch_supported true) in
  let story = Story.set_screen_split_supported story (Screen_split_supported true) in
  let story = Story.set_sound_effects_supported story (Sound_effects_supported false) in
  let story = Story.set_default_is_variable_pitch story (Default_is_variable_pitch false) in
  let story = Story.set_timed_keyboard_supported story (Timed_keyboard_supported false) in

  let story = Story.set_screen_width story (Screen.width screen) in
  let story = Story.set_screen_height story (Screen.height screen) in

  let pc = Story.initial_program_counter story in
  let initial_frame = Frame.make pc in
  {
    story = story;
    program_counter = pc;
    frames = Frameset.make initial_frame;
    random = Randomness.make_random();
    state = Running;
    screen = screen;
    has_new_output = false;
    screen_selected = true;
    transcript = Transcript.empty;
    transcript_selected = Story.transcript_enabled story;
    commands = [];
    commands_selected = false;
    memory_table = [];
    memory_selected = false;
    text_address = Input_buffer 0;
    parse_address = Parse_buffer 0;
    input = "";
    input_max = 0
}

let story interpreter =
  interpreter.story

let state interpreter =
  interpreter.state

let screen interpreter =
  interpreter.screen

let input interpreter =
  interpreter.input

let has_new_output interpreter =
  interpreter.has_new_output

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
  
let read_variable_in_place interpreter variable =
  match variable with
  | Stack -> peek_stack interpreter
  | Local_variable local -> read_local interpreter local
  | Global_variable global -> read_global interpreter global

let read_variable interpreter variable =
  match variable with
  | Stack -> (peek_stack interpreter, pop_stack interpreter)
  | Local_variable local -> (read_local interpreter local, interpreter)
  | Global_variable global -> (read_global interpreter global, interpreter)
  
let write_variable_in_place interpreter variable value =
  match variable with
  | Stack -> push_stack (pop_stack interpreter) value
  | Local_variable local -> write_local interpreter local value
  | Global_variable global -> write_global interpreter global value

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
 (* A call never has a branch and we already know the next pc, so we
 don't need to call interpret_branch here.  *)

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

let split_window interpreter lines=
  { interpreter with screen = Screen.split_window interpreter.screen lines }

let set_status_line interpreter =
  let status = Status_line.make interpreter.story in
  let screen = Screen.set_status interpreter.screen status in
  { interpreter with has_new_output = true; screen }

let display_current_instruction interpreter =
  let address = interpreter.program_counter in
  let instruction = Instruction.decode interpreter.story address in
  Instruction.display instruction interpreter.story

(* Debugging method *)
let display interpreter =
  let frames = Frameset.display_frames interpreter.frames in
  let instr = display_current_instruction interpreter in
  Printf.sprintf "\n---\n%s\n%s\n" frames instr

let select_output_stream interpreter stream value =
  match stream with
  | ScreenStream -> { interpreter with screen_selected = value }
  | TranscriptStream ->
    let value = Transcript_enabled value in
    { interpreter with
    transcript_selected = value;
    story = Story.set_transcript_enabled interpreter.story value }
  | MemoryStream -> failwith "use select/deselect memory stream"
  | CommandStream -> { interpreter with commands_selected = value };;

let select_memory_stream interpreter table =
  { interpreter with
    memory_selected = true;
    memory_table = table :: interpreter.memory_table }

let deselect_memory_stream interpreter =
  match interpreter.memory_table with
  | [] -> { interpreter with memory_selected = false }
  | [_] -> { interpreter with memory_selected = false; memory_table = [] }
  | _ :: t -> { interpreter with memory_selected = true; memory_table = t }

let write_command interpreter input =
  if interpreter.commands_selected then
    {interpreter with commands = (input :: interpreter.commands) }
  else interpreter

let print interpreter text =
  (* TODO: Consider building an output stream manager to provide an
  abstraction of this logic.  *)
  (* If output stream 3 is selected then no output goes to any other
  selected stream *)
  if interpreter.memory_selected then
    let table = List.hd interpreter.memory_table in
    let new_story = Story.write_length_word_prefixed_string interpreter.story table text in
    { interpreter with story = new_story }
  else
    let (Transcript_enabled transcript_enabled) = interpreter.transcript_selected in
    let new_transcript =
      if transcript_enabled then
        Transcript.append interpreter.transcript text
      else interpreter.transcript in
    let new_screen =
      if interpreter.screen_selected then Screen.print interpreter.screen text
      else interpreter.screen in
    { interpreter with
      transcript = new_transcript;
      screen = new_screen;
      has_new_output = interpreter.screen_selected }

(* Handlers for individual instructions

All these handlers take their operands followed by an interpreter
and sometimes an instruction. Those handlers that take just an interpreter
will have the next PC determined by their caller. Those that take both
an interpreter and an instruction take responsibility for doing their
own next-pc logic.

*)

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

(* TODO: Fix up Instruction.display code for these opcodes. *)

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
  let decremented = signed_word (original + 1) in
  let write_interpreter = write_variable_in_place interpreter variable decremented in
  let result = if decremented > value then 1 else 0 in
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

(* Spec: 2OP:17 get_prop object property -> (result)
  Read property from object (resulting in the default value if it had no
  such declared property). If the property has length 1, the value is only
  that byte. If it has length 2, the first two bytes of the property are
  taken as a word value. It is illegal for the opcode to be used if the
  property has length greater than 2, and the result is unspecified. *)

let handle_get_prop obj prop interpreter =
  let obj = Object obj in
  let prop = Property prop in
  Object.property interpreter.story obj prop

(* Spec: 2OP:18 get_prop_addr object property -> (result)
  Get the byte address (in dynamic memory) of the property data for the
  given object's property. This must return 0 if the object hasn't got
  the property. *)

let handle_get_prop_addr obj prop interpreter =
  let obj = Object obj in
  let prop = Property prop in
  let (Property_data addr) = Object.property_address interpreter.story obj prop in
  addr

(* Spec: 2OP:19 get_next_prop object property -> (result)
  Gives the number of the next property provided by the quoted object.
  This may be zero, indicating the end of the property list; if called
  with zero, it gives the first property number present. It is illegal to try
  to find the next property of a property which does not exist, and an
  interpreter should halt with an error message (if it can efficiently check
  this condition). *)

let handle_get_next_prop obj prop interpreter =
  let obj = Object obj in
  let prop = Property prop in
  let (Property next) = Object.next_property interpreter.story obj prop in
  next

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
    interpret_branch store_interpreter instruction result
  else
    let routine_address = Packed_routine routine_address in
    let routine_address = Story.decode_routine_packed_address interpreter.story routine_address in
    let resume_at = Instruction.following instruction in
    let store = Instruction.store instruction in
    let frame = Frame.make_call_frame interpreter.story arguments routine_address resume_at store in
    let pc = Routine.first_instruction interpreter.story routine_address in
    set_program_counter (add_frame interpreter frame) pc

(* Spec: 2OP:27 set_colour foreground background
                set_colour foreground background window
If coloured text is available, set text to be foreground-against-background.
(Flush any buffered text to screen, in the old colours, first.) In version 6,
the window argument is optional and is by default the current window. *)

let handle_set_colour3 foreground background window interpreter =
  (* TODO: set_colour is not yet implemeted. Treat it as a no-op. *)
  interpreter

let handle_set_colour2 foreground background interpreter =
  (* TODO: set_colour is not yet implemeted. Treat it as a no-op. *)
  interpreter

(* Spec: 2OP:28 throw value stack-frame
Opposite of catch: resets the routine call state to the state it had
when the given stack frame value was 'caught', and then returns.
In other words, it returns as if from the routine which executed
the catch which found this stack frame value. *)

let handle_throw value frame_number interpreter =
  failwith "TODO: throw instruction not yet implemented"

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

(* Spec: 1OP:132 get_prop_len property-address -> (result)
  Get length of property data (in bytes) for the given object's property.
  It is illegal to try to find the property length of a property which does
  not exist for the given object, and an interpreter should halt with an error
  message (if it can efficiently check this condition). *)

let handle_get_prop_len property_address interpreter =
  (* TODO: Make a wrapper type for property addresses *)
  let property_address = Property_data property_address in
  Object.property_length_from_address interpreter.story property_address

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
  (* TODO: Add wrapper type for string addresses *)
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

let handle_ret result interpreter instruction =
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

(* Spec:  1OP:143 not value -> (result)
          VAR:248 not value -> (result)
Bitwise NOT (i.e., all 16 bits reversed). Note that in Versions 3 and 4 this
is a 1OP instruction, reasonably since it has 1 operand, but in later versions
it was moved into the extended set to make room for call_1n. *)

(* Note that the spec intended to say "was made into a VAR instruction"; it was
not made an EXT instruction. *)

let handle_not x interpreter =
  lnot x

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

(* Spec: 0OP:180 nop
  Probably the official "no operation" instruction, which, appropriately,
  was never operated (in any of the Infocom datafiles): it may once have
  been a breakpoint. *)

let handle_nop interpreter =
  interpreter


(* Spec: 0OP:181 save ?(label)
         0OP:181 5 4 save -> (result)
On Versions 3 and 4, attempts to save the game (all questions about
filenames are asked by interpreters) and branches if successful. From
Version 5 it is a store rather than a branch instruction; the store value is
0 for failure, 1 for "save succeeded" and 2 for "the game is being restored
and is resuming execution again from here, the point where it was saved".

It is illegal to use this opcode within an interrupt routine (one called
asynchronously by a sound effect, or keyboard timing, or newline counting). *)

(* TODO: Prompt for filename *)
let filename = "FLATHEAD.SAV"
let save_failed = 0
let save_succeeded = 1
let restore_succeeded = 2

let handle_save interpreter =
(* The PC at present points to the save instruction. The convention,
documented nowhere I have found thus far, is to save the PC + 1.  Why
on earth would we do that?

The save instruction has a branch in v3 and a store in v4. When control
resumes *following the restore*, the restore code needs to know
whether to branch / what to store, which is indicated by the second
byte of the save instruction! So we serialize the address of that thing.

So on version 3, what happens is:

* On a failed save the result is 0 so the branch of the save is not taken.
* On a successful save the result is 1, so the branch of the save is taken.
* On a failed restore the result is 0 so the branch of the restore is not taken.
* On a successful restore the result is *2*, but the branch is never taken
  from the restore; the branch is taken from the save.
* Note that this means that in v3, the game has no idea when it resumes
  from the save whether it just completed a successful save, or just
  completed a successful restore. (The *interpreter* knows but the *game*
  does not.)

In version 4:
* On a failed save the result is 0, which is stored.
* On a successful save the result is 1, which is stored.
* On a failed restore the result is 0, which is stored.
* On a successful restore the storage of the restore never happens.
  The result is 2, which is then stored by the back end of the save.
* Now the game can determine whether the save failed (0), succeeded (1)
  or we just restored (2). *)

  let compressed = Compression.compress interpreter.story in
  let release = Story.release_number interpreter.story in
  let serial = Story.serial_number interpreter.story in
  let checksum = Story.header_checksum interpreter.story in
  let (Instruction pc) = interpreter.program_counter in
  let frames = Frameset.make_frameset_record interpreter.frames in
  let form = Quetzal.save release serial checksum (Instruction (pc + 1)) compressed frames in
  Iff.write_iff_file filename form;
  (* TODO: Handle failure *)
  save_succeeded

(* Spec: 0OP:182 restore ?(label)
         0OP:182 restore -> (result)

  In Version 3, the branch is never actually made, since either the game has
  successfully picked up again from where it was saved, or it failed to load
  the save game file.

  As with restart, the transcription and fixed font bits survive.

  The interpreter gives the game a way of knowing that a restore has just
  happened (see save).

  If the restore fails, 0 is returned, but once again this necessarily
  happens since otherwise control is already elsewhere. *)

let handle_restore interpreter instruction =
  (* In versions 1, 2 and 3 the restore branches on failure. In version 4 the
  restore stores a value on failure. Of course if the restore succeeds then
  the interpreter continues with the restored instruction pointer. *)
  (* TODO: Handle exceptions *)
  (* TODO: prompt for a filename *)
  let ifzd = read_iff_file filename ifzd_form in
  let (release_number, serial_number, checksum, program_counter) = Quetzal.read_header ifzd in
  (* TODO: Check the release, serial number, checksum *)
  let frame_records = Quetzal.read_stacks ifzd in
  let frames = Frameset.make_frameset_from_records frame_records in
  let (compressed, uncompressed) = Quetzal.read_memory ifzd in
  (* TODO: Deal with memory size mismatch. *)
  let new_story =
    match (compressed, uncompressed) with
    | (Some bytes, _) -> Compression.apply_compressed_changes interpreter.story bytes
    | (_, Some bytes) -> Compression.apply_uncompressed_changes interpreter.story bytes
    | _ -> failwith "TODO handle failure reading memory" in
  (* TODO: If restore failed then we need to complete the restore instruction
  with result 0. *)
  (* If the restore succeeded then we need to complete the save instruction
    with result 2. See comments in handle_save that describe what is going
    on here. *)
  let save_pc = Instruction (program_counter - 1) in
  let save_instruction = Instruction.decode new_story save_pc in
  if (Instruction.opcode save_instruction) != OP0_181 then
    failwith "Restored PC is not on save instruction";
  let new_interpreter = { interpreter with
    story = new_story;
    program_counter = save_pc;
    frames } in
  (* TODO: All the bits that have to be preserved. Make a common helper
  method with restart. *)
  (* After a restore, redraw the status line *)
  let new_interpreter = set_status_line new_interpreter in
  (* After a restore, collapse the upper window *)
  let new_interpreter = split_window new_interpreter (Character_height 0) in
  (* The save is either a conditional branch or a store depending on the version.
  We'll just do both and let the helper sort it out. *)
  let store = Instruction.store save_instruction in
  let store_interpreter = interpret_store new_interpreter store restore_succeeded in
  let save_interpreter = interpret_branch store_interpreter save_instruction restore_succeeded in
  save_interpreter
(* end of handle_restore *)

(* Spec: 0OP:183 restart
Restart the game. (Any "Are you sure?" question must be asked by the game,
not the interpreter.) The only pieces of information surviving from the
previous state are the "transcribing to printer" bit (bit 0 of 'Flags 2' in
the header, at address $10) and the "use fixed pitch font" bit (bit 1 of
'Flags 2'). In particular, changing the program start address before a
restart will not have the effect of restarting from this new address. *)

let handle_restart interpreter instruction =
  (* If transcripting is active, this has to stay on in
  the restarted interpreter *)
  (* TODO: Other bits that have to stay on *)
  let (Transcript_enabled transcript_on) = interpreter.transcript_selected in
  let transcript = interpreter.transcript in
  let commands = interpreter.commands in
  let story = Story.original interpreter.story in
  let original = make story interpreter.screen in
  let restarted_interpreter = select_output_stream original TranscriptStream transcript_on in
  { restarted_interpreter with transcript; commands }

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

(* Spec: 0OP:186 quit
  Exit the game immediately. (Any "Are you sure?" question must be asked by
  the game, not the interpreter.) It is not legal to return from the main
  routine (that is, from where execution first begins) and this must be
  used instead. *)

let handle_quit interpreter instruction =
  (* We do not advance to the next instruction *)
  { interpreter with state = Halted }

(* Spec: 0OP:187 new_line
  Print carriage return. *)

let handle_new_line interpreter =
  print interpreter "\n"

(* Spec: 0OP:188 show_status
  (In Version 3 only.) Display and update the status line now (don't
  wait until the next keyboard input). (In theory this opcode is illegal
  in later Versions but an interpreter should treat it as nop, because
  Version 5 Release 23 of 'Wishbringer' contains this opcode by accident.) *)

let handle_show_status interpreter =
  set_status_line interpreter

(* Spec:  0OP:189 verify ?(label)
  Verification counts a (two byte, unsigned) checksum of the file from $0040
  onwards (by taking the sum of the values of each byte in the file,
  modulo $10000) and compares this against the value in the game header,
  branching if the two values agree. (Early Version 3 games do not have
  the necessary checksums to make this possible.)

  The interpreter may stop calculating when the file length (as given in the
  header) is reached. It is legal for the file to contain more bytes than
  this, but if so the extra bytes must all be 0, which would contribute
  nothing [to] the checksum anyway. (Some story files are padded out to an exact
  number of virtual-memory pages using 0s.) *)

let handle_verify interpreter =
  if Story.verify_checksum interpreter.story then 1 else 0

(* Spec: 0OP:191 piracy ?(label)
  Branches if the game disc is believed to be genuine by the interpreter
  (which is assumed to have some arcane way of finding out). Interpreters
  are asked to be gullible and to unconditionally branch. *)

let handle_piracy interpreter =
  1

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

(* Spec: VAR:227 put_prop object property value
  Writes the given value to the given property of the given object. If the
  property does not exist for that object, the interpreter should halt with a
  suitable error message. If the property length is 1, then the interpreter
  should store only the least significant byte of the value. (For instance,
  storing -1 into a 1-byte property results in the property value 255.)
  As with get_prop the property length must not be more than 2: if it is,
  the behaviour of the opcode is undefined. *)

let handle_putprop obj prop value interpreter =
  let obj = Object obj in
  let prop = Property prop in
  { interpreter with story = Object.write_property interpreter.story obj prop value }

(* Spec: VAR:234 3 split_window lines
Splits the screen so that the upper window has the given number of lines: or,
if this is zero, unsplits the screen again. In Version 3 (only) the upper
window should be cleared after the split. *)

let handle_split_window lines interpreter =
  (* TODO: in version 3 only, clear the upper window after the split. *)
  let lines = Character_height lines in
  split_window interpreter lines

(* Spec: VAR:228 sread text parse
                 sread text parse time routine
                 aread text parse time routine -> (result)

This is by far the most complex opcode; rather than putting
the specification details here I'll put them inline. *)

let handle_sread2 text_addr parse_addr interpreter instruction =
  (* TODO: make a wrapper type for pointer-to-text-buffer *)
  let parse_addr = Parse_buffer parse_addr in
  (* This instruction is broken up into two halves. The first determines the size of
  the text buffer needed and then gives back an interpreter set to "I need input".
  The second half (above) does the actual work once the host has provided the data. *)
  (* Spec:
  This opcode reads a whole command from the keyboard (no prompt is automatically displayed).
  It is legal for this to be called with the cursor at any position on any window.
  In Versions 1 to 3, the status line is automatically redisplayed first.  *)
  let interpreter = set_status_line interpreter in
  (* Spec:
  In Versions 1 to 4, byte 0 of the text-buffer should initially contain the
  maximum number of letters which can be typed, minus 1 (the interpreter should
  not accept more than this).
  -----
  This part of the spec is extraordinarily hard to understand. Let me try to
  suss it out.  Let's suppose WOLOG we want to allow 100 user-supplied
  characters in the buffer.  So the whole buffer will need to be 102 bytes long.
  Suppose the user has typed in a 100 character string. The buffer layout is:
  * 1 byte containing some number, call it L
  * 100 bytes of user data
  * 1 byte of zero terminator
  Now the question is: what is the number L, stored in the length byte?
  The spec says that L is equal to the maximum number of letters which
  can be typed -- 100 -- minus 1 -- 99.
  This seems crazy. The spec is almost certainly wrong here. The author
  intended to write PLUS ONE, not MINUS ONE.  Under that interpretation,
  L is equal to the maximum number of letters which can be typed -- 100 --
  plus 1 -- 101 -- which gives the actual number of valid bytes that are
  beyond the length byte.

  The spec could be more clearly worded by saying:

  * Byte zero contains L, the maximum number of characters which may be written
  into the buffer starting at byte one. Since the characters must be zero-terminated,
  the maximum number of characters which the interpreter can accept is L - 1.

  Things are seemingly much more straighforward for version 5, but we'll soon
  see that no, it's just as confusing there.
  ----
  Spec:
  In Versions 5 and later, byte 0 of the text-buffer should initially contain
  the maximum number of letters which can be typed (the interpreter should
  not accept more than this).    *)

  let maximum_letters = Story.read_byte interpreter.story (Byte_address text_addr) in
  let maximum_letters =
    if Story.v4_or_lower (Story.version interpreter.story) then maximum_letters - 1
    else maximum_letters in

  (* At this point set the state to "needs input" and return that interpreter.
  The host will get the input and call back to complete the process. *)
  { interpreter with
      state = Waiting_for_input;
      text_address = Input_buffer text_addr;
      parse_address = parse_addr;
      input_max = maximum_letters }
  (* end handle_sread *)

let handle_sread4 text parse time routine interpreter instruction =
  (* TODO: The time routine feature is not yet implemented. Just ignore
  it for now. *)
  (* Spec:
  In Version 4 and later, if the operands time and routine are supplied
  (and non-zero) then the routine call routine() is made every time/10 seconds
  during the keyboard-reading process. If this routine returns true, all input
  is erased (to zero) and the reading process is terminated at once.
  (The terminating character code is 0.) The routine is permitted to print to
  the screen even if it returns false to signal "carry on": the interpreter
  should notice and redraw the input line so far, before input continues. *)
  handle_sread2 text parse interpreter instruction

let complete_sread (Input_buffer text_addr) parse_addr input interpreter instruction =
  (* Spec: The text typed is reduced to lower case *)
  (* Note: it is not clear from reading the spec whether this applies just
  to versions 1-4, or all versions. Let's assume all. *)
  let text = String.lowercase input in
  let story = interpreter.story in
  (* TODO: Could use some helper methods on input buffers here and in tokeniser.*)
  let maximum_letters = Story.read_byte story (Byte_address text_addr) in
  let maximum_letters =
    if Story.v4_or_lower (Story.version interpreter.story) then maximum_letters - 1
    else maximum_letters in
  let trimmed = truncate text maximum_letters in
  let story = Tokeniser.write_user_string_to_memory story (Input_buffer text_addr) trimmed in
  (* Spec:
  Next, lexical analysis is performed on the text (except that in Versions 5
  and later, if parsebuffer is zero then this is omitted). *)
  let story =
    if parse_addr = Parse_buffer 0 then story
    else Tokeniser.lexical_analysis story parse_addr trimmed in
  let interpreter = { interpreter with state = Running } in
  let interpreter = { interpreter with story } in
  let interpreter = write_command interpreter input in
    (* Spec: If input was terminated in the usual way, by the player typing a carriage return, then a carriage
    return is printed (so the cursor moves to the next line). If it was interrupted, the cursor is left at
    the rightmost end of the text typed in so far.*)
  let interpreter = print interpreter (input ^ "\n") in
  let interpreter = { interpreter with screen = Screen.fully_scroll interpreter.screen } in
  (* Spec:  In Version 5 and later, this is a store instruction: the return
    value is the terminating character (note that the user pressing his "enter"
    key may cause either 10 or 13 to be returned; the author recommends that
    interpreters return 10).
    A timed-out input returns 0. *)
  let result = 10 in
  let store = Instruction.store instruction in
  let interpreter = interpret_store interpreter store result in
  interpret_branch interpreter instruction result
  (* End of complete_sread *)

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

(* Spec: VAR:231 random range -> (result)
  If range is positive, returns a uniformly random number between 1 and
  range. If range is negative, the random number generator is seeded to
  that value and the return value is 0. Most interpreters consider giving
  0 as range illegal (because they attempt a division with remainder by the
  range), but correct behaviour is to reseed the generator in as random a
  way as the interpreter can (e.g. by using the time in milliseconds). *)

let handle_random n interpreter =
  if n = 0 then
    (0, { interpreter with random = Randomness.make_random()})
  else if n < 0 then
    (0, { interpreter with random = Randomness.make_seeded n })
  else
    let (result, random) = Randomness.next interpreter.random n in
    (result, { interpreter with random })

(* Spec: VAR:232 push value
Pushes value onto the game stack. *)

let handle_push value interpreter =
  push_stack interpreter value

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

(* Spec: VAR:235 set_window window
  Selects the given window for text output. *)

let handle_set_window window interpreter =
  let window =
    match window with
    | 0 -> Lower_window
    | 1 -> Upper_window
    | _ -> failwith "Unexpected window in set_window" in
  { interpreter with screen = Screen.set_window interpreter.screen window }

(* Spec: VAR:237 erase_window window
    Erases window with given number (to background colour); or if -1 it
    unsplits the screen and clears the lot; or if -2 it clears the screen
    without unsplitting it. In cases -1 and -2, the cursor may move (see
    Section 8 for precise details) *)

let handle_erase_window window interpreter =
  (* In Versions 5 and later, the cursor for the window being erased should
    be moved to the top left. *)
  (* In Version 4, the lower window's cursor moves to its bottom left,
     while the upper window's cursor moves to top left *)
  let window = signed_word window in
  let unsplit = match window with
    | -2 -> interpreter.screen
    | -1 -> Screen.split_window interpreter.screen (Character_height 0)
    | _ -> interpreter.screen in
  let erased = match window with
    | -2
    | -1 -> Screen.erase_all unsplit
    | 0 -> Screen.erase_lower unsplit
    | 1 -> Screen.erase_upper unsplit
    | _ -> failwith "unexpected window number in erase_window" in
  let upper_moved = match window with
    | -2
    | -1
    | 1 -> Screen.set_upper_cursor erased Window.top_left
    | _ -> erased in
  let lower_moved = match window with
    | -2
    | -1
    | 0 ->
      if Story.v4_or_lower (Story.version interpreter.story) then
        Screen.set_lower_cursor_bottom_left upper_moved
      else
        Screen.set_lower_cursor upper_moved Window.top_left
    | _ -> upper_moved in
  { interpreter with screen = lower_moved }

(* Spec: VAR:238 erase_line value
Versions 4 and 5: if the value is 1, erase from the current cursor
position to the end of its line in the current window. If the value
is anything other than 1, do nothing.
Version 6: if the value is 1, erase from the current cursor position to the
end of the its line in the current window. If not, erase the given number of
pixels minus one across from the cursor (clipped to stay inside the right
margin). The cursor does not move. *)

let handle_erase_line value interpreter =
  if value = 1 then
    { interpreter with screen = Screen.erase_line interpreter.screen }
  else
    interpreter

(* Spec: VAR:239 set_cursor line column
                 set_cursor line column window
Move cursor in the current window to the position (x,y) (in units)
relative to (1,1) in the top left.
(In Version 6 the window is supplied and need not be the current one.
Also, if the cursor would lie outside the current margin settings, it
is moved to the left margin of the current line.)

In Version 6, set_cursor -1 turns the cursor off, and either
set_cursor -2 or set_cursor -2 0 turn it back on. It is not known what,
if anything, this second argument means: in all known cases it is 0. *)

let handle_set_cursor2 line column interpreter =
  (* Spec 8.7.2.3
  When the upper window is selected, its cursor position can be moved with
  set_cursor. The opcode has no effect when the lower window is selected.
  It is illegal to move the cursor outside the current size of the upper
  window. *)
  let line = Character_y line in
  let column = Character_x column in
  match Screen.selected_window interpreter.screen with
  | Lower_window -> interpreter
  | Upper_window ->
    let cursor = Cursor (column, line) in
    { interpreter with screen = Screen.set_cursor interpreter.screen cursor }

let handle_set_cursor3 line column window interpreter =
  failwith "TODO: set_cursor with window not yet implemented"

(* Spec: VAR:240 get_cursor array
Puts the current cursor row into the word 0 of the given array, and the
current cursor column into word 1. (The array is not a table and has no
size information in its initial entry.) *)

let handle_get_cursor arr interpreter =
  let arr = Word_address arr in
  let Cursor ((Character_x x),( Character_y y)) = Screen.get_active_cursor interpreter.screen in
  let story = Story.write_word interpreter.story arr y in
  let story = Story.write_word story (inc_word_addr arr) x in
  { interpreter with story }

(* Spec: VAR:241 set_text_style style
  Sets the text style to: Roman (if 0), Reverse Video (if 1),
  Bold (if 2), Italic (4), Fixed Pitch (8). In some interpreters (though
  this is not required) a combination of styles is possible (such as reverse
  video and bold). In these, changing to Roman should turn off all the
  other styles currently set. *)

let handle_set_text_style style interpreter =
  (* TODO: set_text_style not yet implemented; treat as a no-op for now. *)
  interpreter

(* Spec: VAR:242 buffer_mode flag
If set to 1, text output on the lower window in stream
If set to 1, text output on the lower window in stream 1 is buffered up
so that it can be word wrapped properly. If set to 0, it isn't. *)

(* I note that this code implements the spec; did the spec intend to leave
unspecified what happens when the value is neither 0 nor 1? *)
let handle_buffer_mode flag interpreter =
  match flag with
  | 0 -> { interpreter with screen =
    Screen.set_word_wrap interpreter.screen Word_wrap_disabled }
  | 1 -> { interpreter with screen =
    Screen.set_word_wrap interpreter.screen Word_wrap_enabled }
  | _ -> interpreter

(* Spec: VAR:243  output_stream number
                  output_stream number table
                  output_stream number table width

  * If stream is 0, nothing happens.
  * If it is positive, then that stream is selected; if negative, deselected.
  * (Recall that several different streams can be selected at once.)
  * When stream 3 is selected, a table must be given into which text can be printed.
  * The first word always holds the number of characters printed, the actual
    text being stored at bytes table+2 onward.
  * It is not the interpreter's responsibility to worry about the length of
    this table being overrun.
  * In Version 6, a width field may optionally be given: if this is non-zero,
    text will then be justified as if it were in the window with that number
    (if width is positive) or a box "width" pixels wide (if negative). Then the
    table will contain not ordinary text but formatted text: see print_form. *)

let handle_output_stream1 stream interpreter =
  let stream = signed_word stream in
  let new_interpreter = match stream with
  | 0 -> interpreter
  | 1 -> select_output_stream interpreter ScreenStream true
  | -1 -> select_output_stream interpreter ScreenStream false
  | 2 -> select_output_stream interpreter TranscriptStream true
  | -2 -> select_output_stream interpreter TranscriptStream true
  | 3 -> failwith "Illegal to select stream 3 without table "
  | -3 -> deselect_memory_stream interpreter
  | 4 -> select_output_stream interpreter CommandStream true
  | -4 -> select_output_stream interpreter CommandStream true
  | _ -> failwith (Printf.sprintf "Invalid stream %d in output_stream" stream) in
  new_interpreter

let handle_output_stream2 stream table interpreter =
  let table = Word_prefixed_string table in
  if stream = 3 then
    select_memory_stream interpreter table
  else
    handle_output_stream1 stream interpreter

let handle_output_stream3 stream table width interp =
  failwith "TODO handle_output_stream stream table width not yet implemented"

(* Spec: VAR:244 input_stream number
Selects the current input stream. *)

let handle_input_stream number interpreter =
  (* TODO: input_stream not yet implemented; treat as a no-op for now. *)
  interpreter

(* Spec: VAR:245 sound_effect number effect volume routine
  The given effect happens to the given sound number. The low byte of volume
  holds the volume level, the high byte the number of repeats. (The value 255
  means "loudest possible" and "forever" respectively.) (In Version 3, repeats
  are unsupported and the high byte must be 0.)
  Note that sound effect numbers 1 and 2 are bleeps (see S 9) and in these
  cases the other operands must be omitted. Conversely, if any of the
  other operands are present, the sound effect number must be 3 or higher.
  The effect can be: 1 (prepare), 2 (start), 3 (stop), 4 (finish with).
  In Versions 5 and later, the routine is called (with no parameters)
  after the sound has been finished (it has been playing in the background
  while the Z-machine has been working on other things). (This is used by
  'Sherlock' to implement fading in and out, which explains why mysterious
  numbers like $34FB were previously thought to be to do with fading.) The
  routine is not called if the sound is stopped by another sound or by an
  effect 3 call. See the remarks to Section 9 for which forms of this
  opcode were actually used by Infocom. In theory, @sound_effect; (with no
  operands at all) is illegal. However interpreters are asked to
  beep (as if the operand were 1) if possible, and in any case not to halt. *)

let handle_sound_effect args interpreter =
  (* TODO: sound_effect not yet implemented; treat as a no-op for now. *)
  interpreter

(* Spec:  VAR:246 read_char 1 time routine -> (result)
  Reads a single character from input stream 0 (the keyboard).
  The first operand must be 1 (presumably it was provided to support
  multiple input devices, but only the keyboard was ever used).
  time and routine are optional (in Versions 4 and later only) and dealt
  with as in read above. *)

let handle_read_char0 interpreter instruction =
  { interpreter with
      state = Waiting_for_input ;
      input_max = 1 }

let handle_read_char2 routine time interpreter instruction =
  handle_read_char0 interpreter instruction

let complete_read_char interpreter instruction input =
  let result = int_of_char input in
  let interpreter = { interpreter with state = Running } in
  let store = Instruction.store instruction in
  let interpreter = interpret_store interpreter store result in
  interpret_branch interpreter instruction result

(* Spec: VAR:247 scan_table x table len form -> (result)
    Is x one of the words in table, which is len words long? If so,
    return the address where it first occurs and branch.
    If not, return 0 and don't.
    The form is optional (and only used in Version 5?): bit 8 is set for
    words, clear for bytes: the rest contains the length of each
    field in the table. (The first word or byte in each field being the one
    looked at.) Thus $82 is the default. *)

let handle_scan_table3 x table len interpreter =
  let table = Word_address table in
  let rec aux i =
    if i = len then
      Word_address 0
    else
      let addr = inc_word_addr_by table i in
      let y = Story.read_word interpreter.story addr in
      if x = y then addr
      else aux (i + 1) in
  let (Word_address result) = aux 0 in
  result

let handle_scan_table4 x table len form interpreter =
  failwith "TODO scan_table x table len form is not yet implemented"

(* Spec VAR:251 tokenise text parse dictionary flag
  * This performs lexical analysis (see read above).
  * If a non-zero dictionary is supplied, it is used (if not, the ordinary
    game dictionary is).
  * If the flag is set, unrecognised words are not written
    into the parse buffer and their slots are left unchanged: this is
    presumably so that if several tokenise instructions are performed in a
    row, each fills in more slots without wiping those filled by the others.
  * Parsing a user dictionary is slightly different. A user dictionary should
    look just like the main one but need not be alphabetically sorted.
  * If  the number of entries is given as -n, then the interpreter
    reads this as "n entries unsorted". This is very convenient if the table
    is being altered in play: if, for instance, the player is naming things. *)

let handle_tokenise2 text parse interpreter =
  failwith "TODO: tokenise not implemented"

let handle_tokenise4 text parse dictionary flag interpreter =
  failwith "TODO: tokenise text parse dictionary flag not implemented"

(* Spec: VAR:252 encode_text zsciitext length from codedtext
  Translates a ZSCII word to Z-encoded text format (stored at coded-text),
  as if it were an entry in the dictionary. The text begins at from in the
  zscii-text buffer and is length characters long.  *)

let handle_encode_text zsciitext length from codedtext =
  failwith "TODO encode_text not implemented"

(* Spec: VAR:253 copy_table first second size
  * If second is zero, then size bytes of first are zeroed.
  * Otherwise first is copied into second, its length in bytes being the
    absolute value of size
  * The tables are allowed to overlap.
  * If size is positive, the interpreter must copy either forwards or
    backwards so as to avoid corrupting first in the copying process.
  * If size is negative, the interpreter must copy forwards even if this
    corrupts first. *)
let handle_copy_table first second size interpreter =
  failwith "TODO copy_table not implemented"

(* Spec: VAR:254 print_table zscii-text width height skip
Print a rectangle of text on screen spreading right and down from the
current cursor position, of given width and height, from the table of
ZSCII text given. (Height is optional and defaults to 1.) If a skip
value is given, then that many characters of text are skipped over in
between each line and the next. (So one could make this display, for
instance, a 2 by 3 window onto a giant 40 by 40 character graphics map.) *)

let handle_print_table4 text width height skip interpreter =
  failwith "TODO print_table not implemented"

let handle_print_table3 text width height interpreter =
  failwith "TODO print_table not implemented"

let handle_print_table2 text width interpreter =
  failwith "TODO print_table not implemented"

(* Spec: VAR:255 check_arg_count argument-number
  Branches if the given argument-number (counting from 1) has been
  provided by the routine call to the current routine. (This allows
  routines in Versions 5 and later to distinguish between the calls
  routine(1) and routine(1,0), which would otherwise be impossible to
  tell apart.) *)

let handle_check_arg_count number interpreter =
  failwith "TODO check_arg_count not implemented"

(* Spec: EXT:0 save table bytes name -> (result)
         EXT:0 save table bytes name prompt -> (result)
  The extension also has (optional) parameters, which save a region of
  the save area, whose address and length are in bytes, and provides a
  suggested filename: name is a pointer to an array of ASCII characters
  giving this name (as usual preceded by a byte giving the number of characters).
  See Section 7.6.

  As of Standard 1.1 an additional optional parameter, prompt, is allowed on
  Version 5 extended save/restore. This allows a game author to tell the
  interpreter whether it should ask for confirmation of the provided file
  name (prompt=1), or just silently save/restore using the provided filename
  (prompt=0). If the parameter is not provided, whether to prompt or not is
  a matter for the interpreter - this might be globally user-configurable.
  Infocom's interpreters do prompt for filenames, many modern ones do not.  *)

let handle_save3 table bytes name interpreter =
    failwith "TODO: save table bytes name not yet implemented "

let handle_save4 table bytes name interpreter prompt =
    failwith "TODO: save table bytes name not yet implemented "

(* Spec:  EXT:1 restore table bytes name -> (result)
          EXT:1 restore table bytes name prompt -> (result)
From Version 5 it can have optional parameters as save does, and returns
the number of bytes loaded if so. *)

let handle_restore3 table bytes name interpreter =
    failwith "TODO: restore table bytes name not yet implemented "

let handle_restore4 table bytes name interpreter prompt =
    failwith "TODO: restore table bytes name not yet implemented "

(* Spec: EXT:2 log_shift number places -> (result)
    Does a logical shift of number by the given number of places,
    shifting left (i.e. increasing) if places is positive, right if negative.
    In a right shift, the sign is zeroed instead of being shifted on.
    (See also art_shift.) *)
    
let handle_log_shift number places interpreter =
  let places = signed_word places in
  if places < 0 then number lsr ( - places)
  else if places > 0 then number lsl places
  else number

(* Spec:  EXT:3 art_shift number places -> (result)
Does an arithmetic shift of number by the given number of places, shifting
left (i.e. increasing) if places is positive, right if negative. In a
right shift, the sign bit is preserved as well as being
shifted on down. (The alternative behaviour is log_shift.) *)

let handle_art_shift number places interpreter =
  let number = signed_word number in
  let places = signed_word places in
  if places < 0 then number asr ( - places)
  else if places > 0 then number lsl places
  else number 

(* Spec: EXT:4 set_font font -> (result)
  If the requested font is available, then it is chosen for the current
  window, and the store value is the font ID of the previous font (which is
  always positive). If the font is unavailable, nothing will happen and the
  store value is 0. *)
let handle_set_font font interpreter =
  (* TODO: set_font not yet implemented; just make it a no-op *)
  (0, interpreter)

(* Spec: EXT:5 draw_picture picture-number y x
  * Displays the picture with the given number.
  * (y,x) coordinates (of the top left of the picture) are each optional, in
    that a value of zero for y or x means the cursor y or x coordinate in
    the current window.
  * It is illegal to call this with an invalid picture number. *)

let handle_draw_picture number y x interpreter =
  failwith "TODO: draw_picture not yet implemented"

(* Spec EXT:9 save_undo -> (result)
Like save, except that the optional parameters may not be specified: it
saves the game into a cache of memory held by the interpreter. If the
interpreter is unable to provide this feature, it must return -1: otherwise
it returns the save return value *)

let handle_save_undo interpreter =
  (* TODO: save_undo NYI, so just return -1 *)
  -1

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
  | (OP2_17, [obj; prop]) -> value (handle_get_prop obj prop)
  | (OP2_18, [obj; prop]) -> value (handle_get_prop_addr obj prop)
  | (OP2_19, [obj; prop]) -> value (handle_get_next_prop obj prop)
  | (OP2_20, [a; b]) -> value (handle_add a b)
  | (OP2_21, [a; b]) -> value (handle_sub a b)
  | (OP2_22, [a; b]) -> value (handle_mul a b)
  | (OP2_23, [a; b]) -> value (handle_div a b)
  | (OP2_24, [a; b]) -> value (handle_mod a b)
  | (OP2_25, [routine; arg1]) -> handle_call routine [arg1] interpreter instruction
  | (OP2_26, [routine; arg1]) -> handle_call routine [arg1] interpreter instruction
  | (OP2_27, [foreground; background]) -> effect (handle_set_colour2 foreground background)
  | (OP2_27, [foreground; background; window]) -> effect (handle_set_colour3 foreground background window)
  | (OP2_28, [x; frame]) -> handle_throw x frame interpreter
  | (OP1_128, [a]) -> value (handle_jz a)
  | (OP1_129, [obj]) -> value (handle_get_sibling obj)
  | (OP1_130, [obj]) -> value (handle_get_child obj)
  | (OP1_131, [obj]) -> value (handle_get_parent obj)
  | (OP1_132, [property_address]) -> value (handle_get_prop_len property_address)
  | (OP1_133, [variable]) -> effect (handle_inc variable)
  | (OP1_134, [variable]) -> effect (handle_dec variable)
  | (OP1_135, [address]) -> effect (handle_print_addr address)
  | (OP1_136, [routine]) -> handle_call routine [] interpreter instruction
  | (OP1_137, [obj]) -> effect (handle_remove_obj obj)
  | (OP1_138, [obj]) -> effect (handle_print_obj obj)
  | (OP1_139, [result]) -> handle_ret result interpreter instruction
  | (OP1_140, [offset]) -> handle_jump offset interpreter instruction
  | (OP1_141, [paddr]) -> effect (handle_print_paddr paddr)
  | (OP1_142, [variable]) -> value (handle_load variable)
  | (OP1_143, [x]) ->
    if Story.v4_or_lower (Story.version interpreter.story) then value (handle_not x)
    else handle_call x [] interpreter instruction
  | (OP0_176, []) -> handle_rtrue interpreter instruction
  | (OP0_177, []) -> handle_rfalse interpreter instruction
  | (OP0_178, []) -> handle_print interpreter instruction
  | (OP0_179, []) -> handle_print_ret interpreter instruction
  | (OP0_180, []) -> effect handle_nop
  | (OP0_181, []) -> value handle_save
  | (OP0_182, []) -> handle_restore interpreter instruction
  | (OP0_183, []) -> handle_restart interpreter instruction
  | (OP0_184, []) -> handle_ret_popped interpreter instruction
  | (OP0_185, []) ->
    if Story.v4_or_lower (Story.version interpreter.story) then effect handle_pop
    else value handle_catch
  | (OP0_186, []) -> handle_quit interpreter instruction
  | (OP0_187, []) -> effect handle_new_line
  | (OP0_188, []) -> effect handle_show_status
  | (OP0_189, []) -> value handle_verify
  (* 190 is the extended bytecode marker *)
  | (OP0_191, []) -> value handle_piracy
  | (VAR_224, routine :: args) -> handle_call routine args interpreter instruction
  | (VAR_225, [arr; ind; value]) -> effect (handle_storew arr ind value)
  | (VAR_226, [arr; ind; value]) -> effect (handle_storeb arr ind value)
  | (VAR_227, [obj; prop; value]) -> effect (handle_putprop obj prop value)
  | (VAR_228, [text; parse]) -> handle_sread2 text parse interpreter instruction
  | (VAR_228, [text; parse; time; routine]) -> handle_sread4 text parse time routine interpreter instruction
  | (VAR_229, [code]) -> effect (handle_print_char code)
  | (VAR_230, [number]) -> effect (handle_print_num number)
  | (VAR_231, [range]) -> interpret_instruction (handle_random range)
  | (VAR_232, [x]) -> effect (handle_push x)
  | (VAR_233, []) -> interpret_instruction handle_pull0
  | (VAR_233, [x]) -> interpret_instruction (handle_pull1 x)
  | (VAR_234, [lines]) -> effect (handle_split_window lines)
  | (VAR_235, [window]) -> effect (handle_set_window window)
  | (VAR_236, routine :: args) -> handle_call routine args interpreter instruction
  | (VAR_237, [window]) -> effect (handle_erase_window window)
  | (VAR_238, [x]) -> effect (handle_erase_line x)
  | (VAR_239, [line; column]) -> effect (handle_set_cursor2 line column)
  | (VAR_239, [line; column; window]) -> effect (handle_set_cursor3 line column window)
  | (VAR_240, [arr]) -> effect (handle_get_cursor arr)
  | (VAR_241, [style]) -> effect (handle_set_text_style style)
  | (VAR_242, [flag]) -> effect (handle_buffer_mode flag)
  | (VAR_243, [number]) -> effect (handle_output_stream1 number)
  | (VAR_243, [number; table]) -> effect (handle_output_stream2 number table)
  | (VAR_243, [number; table; width]) -> effect (handle_output_stream3 number table width)
  | (VAR_244, [number]) -> effect (handle_input_stream number)
  | (VAR_245, args) -> effect (handle_sound_effect args)
  | (VAR_246, [dummy]) -> handle_read_char0 interpreter instruction
  | (VAR_246, [dummy; time; routine]) -> handle_read_char2 time routine interpreter instruction
  | (VAR_247, [x; table; len]) -> value (handle_scan_table3 x table len)
  | (VAR_247, [x; table; len; form]) -> value (handle_scan_table4 x table len form)
  | (VAR_248, [x]) -> value (handle_not x)
  | (VAR_249, routine :: args) -> handle_call routine args interpreter instruction
  | (VAR_250, routine :: args) -> handle_call routine args interpreter instruction
  | (VAR_251, [text; parse]) -> effect (handle_tokenise2 text parse)
  | (VAR_251, [text; parse; dictionary; flag]) -> effect (handle_tokenise4 text parse dictionary flag)
  | (VAR_252, [zsciitext; length; from; codedtext]) -> effect (handle_encode_text zsciitext length from codedtext)
  | (VAR_253, [first; second; size]) -> effect (handle_copy_table first second size)
  | (VAR_254, [text; width; height; skip]) -> effect (handle_print_table4 text width height skip)
  | (VAR_254, [text; width; height]) -> effect (handle_print_table3 text width height)
  | (VAR_254, [text; width]) -> effect (handle_print_table2 text width)
  | (VAR_255, [number]) -> value (handle_check_arg_count number)
  | (EXT_0, []) -> value handle_save
  | (EXT_0, [table; bytes; name]) -> value (handle_save3 table bytes name)
  | (EXT_0, [table; bytes; name; prompt]) -> value (handle_save4 table bytes name prompt)
  | (EXT_1, []) -> handle_restore interpreter instruction
  | (EXT_1, [table; bytes; name]) -> interpret_instruction (handle_restore3 table bytes name)
  | (EXT_1, [table; bytes; name; prompt]) -> interpret_instruction (handle_restore4 table bytes name prompt)
  | (EXT_2, [number; places]) -> value (handle_log_shift number places)
  | (EXT_3, [number; places]) -> value (handle_art_shift number places)
  | (EXT_4, [font]) -> interpret_instruction (handle_set_font font)
  | (EXT_5, [number; y; x]) -> effect (handle_draw_picture number y x)

  | (EXT_9 , []) -> value handle_save_undo

  | (EXT_6 , _)
  | (EXT_7 , _)
  | (EXT_8 , _)
  | (EXT_10, _)
  | (EXT_11, _)
  | (EXT_12, _)
  | (EXT_13, _)
  | (EXT_14, _)
  | (EXT_16, _)
  | (EXT_17, _)
  | (EXT_18, _)
  | (EXT_19, _)
  | (EXT_20, _)
  | (EXT_21, _)
  | (EXT_22, _)
  | (EXT_23, _)
  | (EXT_24, _)
  | (EXT_25, _)
  | (EXT_26, _)
  | (EXT_27, _)
  | (EXT_28, _)
  | (EXT_29, _)   -> failwith (Printf.sprintf "TODO: %s " (Instruction.display instruction interpreter.story))

  | _ -> failwith (Printf.sprintf "unexpected instruction %s" (Instruction.display instruction interpreter.story))
  (* End step_instruction *)

(* Steps the interpreter to its next public-facing state. However this need
not be a step to the next instruction. An interpreter which is waiting for
input or producing a large output may take several steps to get to the
next instruction. If an interpreter is waiting for input then you must
call step_with_input.  *)

let step interpreter =
  if interpreter.state = Halted then
    failwith "interpreter is halted";
  if interpreter.state = Waiting_for_input then
    failwith "interpreter is waiting for input";
  let screen =
    if Screen.needs_more interpreter.screen then
      Screen.clear_more interpreter.screen
    else
      interpreter.screen in
  if Screen.needs_scroll screen then
    { interpreter with screen = Screen.scroll screen; has_new_output = true }
  else
    step_instruction { interpreter with screen; has_new_output = false }

let step_with_input interpreter key =
  let key_text = string_of_char key in
  let length = String.length interpreter.input in
  let instruction =
    Instruction.decode interpreter.story interpreter.program_counter in
  let handle_enter () =
    let blank_input = { interpreter with input = ""; input_max = 0; text_address = Input_buffer 0; parse_address = Parse_buffer 0 } in
    complete_sread interpreter.text_address interpreter.parse_address interpreter.input blank_input instruction  in
  let handle_backspace () =
    if length = 0 then
      interpreter
    else
      { interpreter with input = truncate interpreter.input (length - 1)} in
  let opcode = Instruction.opcode instruction in
  match opcode with
  | VAR_246 -> complete_read_char interpreter instruction key
  | VAR_228 ->
    if key_text = "\r" then handle_enter()
    else if key_text = "\b" then handle_backspace()
    else if length >= interpreter.input_max then interpreter
    else { interpreter with input = interpreter.input ^ key_text }
  | _ -> failwith "not waiting for input"
