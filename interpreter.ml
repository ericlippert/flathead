open Utility
open Story
open Instruction
open Window
open Screen
open Iff
open Quetzal

type state =
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
  program_counter : int;
  frames : Frameset.t;
  random : Randomness.t;
  state : state;

  (* output stream 1 *)
  screen : Screen.t;
  has_new_output : bool;
  screen_selected : bool;

  (* output stream 2 *)
  transcript : Transcript.t;
  transcript_selected : bool;

  memory_table : int list;
  memory_selected : bool;

  (* output stream 4 *)
  commands : string list;
  commands_selected : bool;

  (* TODO: Other input streams *)
  input : string;
  input_max : int;
}

let make story screen =
  (* TODO: Restore these after a restart / restore *)
  let story = set_screen_width story screen.width in
  let story = set_screen_height story screen.height in
  let story = set_supports_multiple_windows story true in
  let pc = initial_program_counter story in
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
    transcript_selected = get_transcript_flag story;
    commands = [];
    commands_selected = false;
    memory_table = [];
    memory_selected = false;
    input = "";
    input_max = 0
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

let set_program_counter interpreter new_program_counter =
  { interpreter with program_counter = new_program_counter }

let read_local interpreter local =
  Frameset.read_local interpreter.frames local

let write_local interpreter local value =
  { interpreter with frames = Frameset.write_local interpreter.frames local value }

let read_global interpreter global =
  Story.read_global interpreter.story global

let write_global interpreter global value =
  { interpreter with story = write_global interpreter.story global value }

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

let interpret_store interpreter instruction result =
  match instruction.store with
  | None -> interpreter
  | Some variable -> write_variable interpreter variable result

let interpret_return interpreter instruction value =
(* TODO: Clean this up to not be so much reading from frame's members.  *)
 let frame = current_frame interpreter in
 let next_pc = frame.Frame.resume_at in
 let store = frame.Frame.store in
 let pop_frame_interpreter = remove_frame interpreter in
 let result_interpreter = set_program_counter pop_frame_interpreter next_pc in
 let store_interpreter =
   match store with
   | None -> result_interpreter
   | Some variable -> write_variable result_interpreter variable value in
 (* A call never has a branch and we already know the next pc *)
 store_interpreter

let interpret_branch interpreter instruction result =
  let next_instruction () =
    let addr = interpreter.program_counter + instruction.length in
    set_program_counter interpreter addr in
  match instruction.branch with
  | None -> next_instruction ()
  | Some (sense, Return_false) ->
    if (result <> 0) = sense then interpret_return interpreter instruction 0
    else next_instruction ()
  | Some (sense, Return_true) ->
    if (result <> 0) = sense then interpret_return interpreter instruction 1
    else next_instruction ()
  | Some (sense, Branch_address branch_target) ->
    if (result <> 0) = sense then set_program_counter interpreter branch_target
    else next_instruction ()

let interpret_instruction interpreter instruction handler =
  let (result, handler_interpreter) = handler interpreter in
  let store_interpreter = interpret_store handler_interpreter instruction result in
  interpret_branch store_interpreter instruction result

let interpret_value_instruction interpreter instruction handler =
  let result = handler interpreter in
  let store_interpreter = interpret_store interpreter instruction result in
  interpret_branch store_interpreter instruction result

let interpret_effect_instruction interpreter instruction handler =
  let handler_interpreter = handler interpreter in
  let result = 0 in
  let store_interpreter = interpret_store handler_interpreter instruction result in
  interpret_branch store_interpreter instruction result

let interpreter_print interpreter text =
  (* TODO: Consider building an output stream manager to provide an
  abstraction of this logic.  *)
  (* If output stream 3 is selected then no output goes to any other
  selected stream *)
  if interpreter.memory_selected then
    let table = List.hd interpreter.memory_table in
    let new_story = write_length_prefixed_string interpreter.story table text in
    { interpreter with story = new_story }
  else
    let new_transcript =
      if interpreter.transcript_selected then
        Transcript.append interpreter.transcript text
      else interpreter.transcript in
    let new_screen =
      if interpreter.screen_selected then Screen.print interpreter.screen text
      else interpreter.screen in
    { interpreter with
      transcript = new_transcript;
      screen = new_screen;
      has_new_output = interpreter.screen_selected }

(* Handlers for individual instructions *)

(* Spec: 2OP:1 je a b ?label
Jump if a is equal to any of the subsequent operands. (Thus @je a never
jumps and @je a b jumps if a = b.) *)

(* Note: Already we are off to a bad start; the revision to the spec says:

je can take between 2 and 4 operands. je with just 1 operand is not permitted.

Note that je is one of the rare "2OP" instructions that can take 3 or 4
operands. *)

let handle_je2 a b interpreter =
  let a = signed_word a in
  let b = signed_word b in
  if a = b then 1 else 0

let handle_je3 a b c interpreter =
  let a = signed_word a in
  let b = signed_word b in
  let c = signed_word c in
  if a = b || a = c then 1 else 0

let handle_je4 a b c d interpreter =
  let a = signed_word a in
  let b = signed_word b in
  let c = signed_word c in
  let d = signed_word d in
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
  let variable = decode_variable variable in
  let value = signed_word value in
  let (original, read_interpreter) = read_variable interpreter variable in
  let original = signed_word original in
  let decremented = signed_word (original - 1) in
  let write_interpreter = write_variable read_interpreter variable decremented in
  let result = if decremented < value then 1 else 0 in
  (result, write_interpreter)

(* Spec: 2OP:5 inc_chk (variable) value ?(label)
  Increment variable, and branch if now greater than value. *)

let handle_inc_chk variable value interpreter =
  let variable = decode_variable variable in
  let value = signed_word value in
  let (original, read_interpreter) = read_variable interpreter variable in
  let original = signed_word original in
  let decremented = signed_word (original + 1) in
  let write_interpreter = write_variable read_interpreter variable decremented in
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
  let parent = object_parent interpreter.story obj1 in
  if parent = obj2 then 1 else 0

(* Spec: 2OP:7 test bitmap flags ?(label)
  Jump if all of the flags in bitmap are set (i.e. if bitmap & flags == flags) *)

let handle_test bitmap flags interpreter =
  let bitmap = unsigned_word bitmap in
  let flags = unsigned_word flags in
  if (bitmap land flags) = flags then 1 else 0

(* Spec: 2OP:8 8 or a b -> (result)
  Bitwise OR. *)

let handle_or a b interpreter =
  let a = unsigned_word a in
  let b = unsigned_word b in
  a lor b

(* Spec: 2OP:9 and a b -> (result)
Bitwise AND. *)

let handle_and a b interpreter =
  let a = unsigned_word a in
  let b = unsigned_word b in
  a land b

(* Spec: 2OP:10 test_attr object attribute ?(label)
  Jump if object has attribute. *)

let handle_test_attr obj attr interpreter =
  let obj = Object obj in
  let attr = unsigned_word attr in (* TODO: Wrapper type for attribute numbers *)
  if object_attribute interpreter.story obj attr then 1 else 0

(* Spec:  2OP:11 set_attr object attribute
  Make object have the attribute numbered attribute. *)

let handle_set_attr obj attr interpreter =
  let obj = Object obj in
  let attr = unsigned_word attr in (* TODO: Wrapper type for attribute numbers *)
  { interpreter with story = set_object_attribute interpreter.story obj attr }

(* Spec: 2OP:12 clear_attr object attribute
  Make object not have the attribute numbered attribute. *)

let handle_clear_attr obj attr interpreter =
  let obj = Object obj in
  let attr = unsigned_word attr in (* TODO: Wrapper type for attribute numbers *)
  { interpreter with story = clear_object_attribute interpreter.story obj attr }

(* Spec: 2OP:13 store (variable) value
  Set the variable referenced by the operand to value *)

(* This is one of those odd instructions like dec_chk (described above)
that takes a variable number as an operand. *)

let handle_store variable value interpreter =
  let variable = decode_variable variable in
  let value = unsigned_word value in
  write_variable interpreter variable value

(* Spec: 2OP:14 insert_obj object destination
  Moves object O to become the first child of the destination object D.
  (Thus, after the operation the child of D is O, and the sibling of O
  is whatever was previously the child of D.) All children of O move with it.
  (Initially O can be at any point in the object tree; it may legally
  have parent zero.) *)

let handle_insert_obj obj destination interpreter =
  let obj = Object obj in
  let destination = Object destination in
  { interpreter with story = insert_object interpreter.story obj destination }

(* Spec: 2OP:15 loadw array word-index -> (result)
Stores array-->word-index (i.e., the word at address array+2*word-index,
which must lie in static or dynamic memory). *)

let handle_loadw arr idx interpreter =
  let arr = unsigned_word arr in
  let idx = unsigned_word idx in
  read_word interpreter.story (arr + idx * 2)

(* Spec: 2OP:16 loadb array byte-index -> (result)
Stores array->byte-index (i.e., the byte at address array+byte-index,
which must lie in static or dynamic memory). *)

let handle_loadb arr idx interpreter =
  let arr = unsigned_word arr in
  let idx = unsigned_word idx in
  read_byte interpreter.story (arr + idx)

(* Spec: 2OP:17 get_prop object property -> (result)
  Read property from object (resulting in the default value if it had no
  such declared property). If the property has length 1, the value is only
  that byte. If it has length 2, the first two bytes of the property are
  taken as a word value. It is illegal for the opcode to be used if the
  property has length greater than 2, and the result is unspecified. *)

let handle_get_prop obj prop interpreter =
  let obj = Object obj in
  let prop = Property prop in
  object_property interpreter.story obj prop

(* Spec: 2OP:18 get_prop_addr object property -> (result)
  Get the byte address (in dynamic memory) of the property data for the
  given object's property. This must return 0 if the object hasn't got
  the property. *)

let handle_get_prop_addr obj prop interpreter =
  let obj = Object obj in
  let prop = Property prop in
  property_address interpreter.story obj prop

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
  let (Property next) = get_next_property interpreter.story obj prop in
  next

(* Spec: 2OP:20 add a b -> (result)
  Signed 16-bit addition. *)

let handle_add a b interpreter =
  let a = signed_word a in
  let b = signed_word b in
  signed_word (a + b)

(* Spec: 2OP:21 sub a b -> (result)
  Signed 16-bit subtraction. *)

let handle_sub a b interpreter =
  let a = signed_word a in
  let b = signed_word b in
  signed_word (a - b)

(* Spec: 2OP:22 mul a b -> (result)
  Signed 16-bit multiplication. *)

let handle_mul a b interpreter =
  let a = signed_word a in
  let b = signed_word b in
  signed_word (a * b)

(* Spec: 2OP:23 div a b -> (result)
  Signed 16-bit division.  Division by zero should halt
  the interpreter with a suitable error message. *)

let handle_div a b interpreter =
  let a = signed_word a in
  let b = signed_word b in
  signed_word (a / b)

(* Spec: 2OP:24 mod a b -> (result)
  Remainder after signed 16-bit division. Division by zero should halt
  the interpreter with a suitable error message. *)

let handle_mod a b interpreter =
  let a = signed_word a in
  let b = signed_word b in
  signed_word (a mod b)


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
  let routine_address = decode_routine_packed_address interpreter.story routine_address in
  if routine_address = 0 then
  (* Spec: When the address 0 is called as a routine, nothing happens and the
     return value is false. *)
    let result = 0 in
    let store_interpreter = interpret_store interpreter instruction result in
    interpret_branch store_interpreter instruction result
  else
    let resume_at = instruction.address + instruction.length in
    let frame = Frame.make_call_frame interpreter.story arguments routine_address resume_at instruction.store in
    let pc = first_instruction interpreter.story routine_address in
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
  let a = unsigned_word a in
  if a = 0 then 1 else 0

(* Spec: 1OP:129 get_sibling object -> (result) ?(label)
  Get next object in tree, branching if this exists, i.e. is not 0 *)

let handle_get_sibling obj interpreter =
  let obj = Object obj in
  let (Object sibling) = object_sibling interpreter.story obj in
  sibling

(* Spec: 1OP:130 get_child object -> (result) ?(label)
  Get first object contained in given object, branching if this exists,
  i.e., is not 0 *)

let handle_get_child obj interpreter =
  let obj = Object obj in
  let (Object child) = object_child interpreter.story obj in
  child

(* Spec: 1OP:131 get_parent object -> (result)
  Get parent object (note that this has no "branch if exists" clause). *)

let handle_get_parent obj interpreter =
  let obj = Object obj in
  let (Object parent) = object_parent interpreter.story obj in
  parent

(* Spec: 1OP:132 get_prop_len property-address -> (result)
  Get length of property data (in bytes) for the given object's property.
  It is illegal to try to find the property length of a property which does
  not exist for the given object, and an interpreter should halt with an error
  message (if it can efficiently check this condition). *)

let handle_get_prop_len property_address interpreter =
  (* TODO: Make a wrapper type for property addresses *)
  let property_address = unsigned_word property_address in
  property_length_from_address interpreter.story property_address

(* Spec: 1OP:133 inc (variable)
  Increment variable by 1. (This is signed, so -1 increments to 0.)

Note that this is another of those unusual instructions that takes as an
argument the number of a variable. *)

let handle_inc variable interpreter =
  let variable = decode_variable variable in
  let (original, read_interpreter) = read_variable interpreter variable in
  let incremented = signed_word (original + 1) in
  write_variable read_interpreter variable incremented

(* Spec: 1OP:134 dec (variable)
  Decrement variable by 1. This is signed, so 0 decrements to -1.

  Note that this is another of those unusual instructions that takes as an
  argument the number of a variable. *)

let handle_dec variable interpreter =
  let variable = decode_variable variable in
  let (original, read_interpreter) = read_variable interpreter variable in
  let decremented = signed_word (original - 1) in
  write_variable read_interpreter variable decremented

(* Spec: 1OP:135 print_addr byte-address-of-string
  Print (Z-encoded) string at given byte address, in dynamic or static memory *)

let handle_print_addr addr interpreter =
  (* TODO: Add wrapper type for string addresses *)
  let addr = unsigned_word addr in
  let text = read_zstring interpreter.story addr in
  interpreter_print interpreter text

(* Spec: 1OP:137 remove_obj object
Detach the object from its parent, so that it no longer has any parent.
(Its children remain in its possession.) *)
let handle_remove_obj obj interpreter =
  let obj = Object obj in
  { interpreter with story = remove_object interpreter.story obj}

(* Spec: 1OP:138 print_obj object
  Print short name of object (the Z-encoded string in the object header,
  not a property). If the object number is invalid, the interpreter should
  halt with a suitable error message. *)

let handle_print_obj obj interpreter =
  let obj = Object obj in
  let text = object_name interpreter.story obj in
  interpreter_print interpreter text


(* Spec:  1OP:143 not value -> (result)
          VAR:248 not value -> (result)
Bitwise NOT (i.e., all 16 bits reversed). Note that in Versions 3 and 4 this
is a 1OP instruction, reasonably since it has 1 operand, but in later versions
it was moved into the extended set to make room for call_1n. *)

(* Note that the spec intended to say "was made into a VAR instruction"; it was
not made an EXT instruction. *)

let handle_not x interpreter =
  let x = unsigned_word x in
  unsigned_word (lnot x)








(*
   Z-machine instructions essentially have three parts. The first part evaluates the
   operands. The second part either causes a side effect or computes a result.
   The third part stores the result and computes what instruction to run next.

   Almost all instructions do the three parts in that order. But there are some
   weird ones:

   * Call does the first two parts, but the third is done by the return. The
     call stores into the frame the information necessary to do the store.

   * A return of course has no third part; it's third part is that of the call

   * A successful restore will pick up where the save left off, which means it
     needs to do the branch or store of the save. See comments in save / restore.

   * A throw is essentially a return that skips over frames.

*)



let handle_store_and_branch interpreter instruction result =
  let store_interpreter =
    match instruction.store with
    | None -> interpreter
    | Some variable -> write_variable interpreter variable result in
  interpret_branch store_interpreter instruction result









let handle_pull interpreter instruction =
  (* TODO: Variadic in v6 *)
  match instruction.operands with
  | [variable_operand] ->
    let (variable, variable_interpreter) = read_operand interpreter variable_operand in
    let target = decode_variable variable in
    let value = peek_stack variable_interpreter in
    let popped_interpreter = pop_stack interpreter in
    let store_interpreter = write_variable popped_interpreter target value in
    interpret_branch store_interpreter instruction 0
  | _ -> failwith "pull requires a variable "


let select_output_stream interpreter stream value =
  match stream with
  | ScreenStream -> { interpreter with screen_selected = value }
  | TranscriptStream -> { interpreter with
    transcript_selected = value;
    story = set_transcript_flag interpreter.story value }
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


let set_status_line interpreter =
  let status = Status_line.make interpreter.story in
  let screen = { interpreter.screen with status } in
  { interpreter with has_new_output = true; screen }

let complete_sread interpreter instruction input =
  (* TODO: Get word separator list from story *)

  (* Returns a list of tuples containing each word, the start location
  in the input string and the address of the matching dictionary word. *)
  let tokenise text interp =
    let length = String.length text in

    let rec find_space_or_end i =
      if i = length then i
      else if text.[i] = ' ' then i
      else find_space_or_end (i + 1) in

    let rec skip_spaces i =
      if i = length then i
      else if text.[i] = ' ' then skip_spaces (i + 1)
      else i in

    let rec token start =
      if start = length then
        None
      else
        let end_of_token = find_space_or_end start in
        let token_text = String.sub text start (end_of_token - start) in
        let dictionary_address = dictionary_lookup interp.story token_text in
        Some (token_text, start, dictionary_address) in

    let rec aux i acc =
      match token i with
      | None -> acc
      | Some (tok, start, addr) ->
        let token_length = String.length tok in
        let next_non_space = skip_spaces (i + token_length) in
        let new_acc = (tok, start, addr) :: acc in
        aux next_non_space new_acc in
      List.rev (aux (skip_spaces 0) []) in
      (* End of tokenise*)

  (* We are no longer waiting for input *)
  let running_interpreter = { interpreter with state = Running } in

  let (text_address, parse_address, operands_interpreter) =
    match instruction.operands with
    | [x_operand; y_operand] ->
      let (x, x_interpreter) = read_operand running_interpreter x_operand in
      let (y, y_interpreter) = read_operand x_interpreter y_operand in
      (x, y, y_interpreter)
    | _ -> failwith (Printf.sprintf "instruction at %04x must have two operands" instruction.address ) in

  let text = String.lowercase input in
  let maximum_letters = read_byte operands_interpreter.story text_address in
  let trimmed = truncate text maximum_letters in
  let copied_story =
    write_string operands_interpreter.story (text_address + 1) trimmed in
  let string_copied_interpreter =
    { operands_interpreter with story = copied_story} in

  (*
  TODO: This section only relevant to V4 and greater

  In Versions 5 and later, byte 0 of the text-buffer should initially contain the maximum number
  of letters which can be typed (the interpreter should not accept more than this). The interpreter
  stores the number of characters actually typed in byte 1 (not counting the terminating character),
  and the characters themselves in bytes 2 onward (not storing the terminating character). (Some
  interpreters wrongly add a zero byte after the text anyway, so it is wise for the buffer to contain
  at least n+3 bytes.)

  Moreover, if byte 1 contains a positive value at the start of the input, then read assumes that
  number of characters are left over from an interrupted previous input, and writes the new characters
  after those already there. Note that the interpreter does not redisplay the characters left
  over: the game does this, if it wants to. This is unfortunate for any interpreter wanting to give input
  text a distinctive appearance on-screen, but 'Beyond Zork', 'Zork Zero' and 'Shogun' clearly
  require it. ("Just a tremendous pain in my butt" -- Andrew Plotkin; "the most unfortunate feature
  of the Z-machine design" -- Stefan Jokisch.)

  In Version 4 and later, if the operands time and routine are supplied (and non-zero) then the
  routine call routine() is made every time/10 seconds during the keyboard-reading process. If this
  routine returns true, all input is erased (to zero) and the reading process is terminated at once.
  (The terminating character code is 0.) The routine is permitted to print to the screen even if it
  returns false to signal "carry on": the interpreter should notice and redraw the input line so far,
  before input continues. (Frotz notices by looking to see if the cursor position is at the left-hand
  margin after the interrupt routine has returned.)

    *)

  (*
  If input was terminated in the usual way, by the player typing a carriage return, then a carriage
  return is printed (so the cursor moves to the next line). If it was interrupted, the cursor is left at
  the rightmost end of the text typed in so far.*)

  let commands_interpreter =
    if string_copied_interpreter.commands_selected then
      {string_copied_interpreter with
        commands = input :: string_copied_interpreter.commands }
    else string_copied_interpreter in
  let printed_interpreter =
    interpreter_print commands_interpreter (input ^ "\n") in
  let new_screen_interpreter = { printed_interpreter with
    screen = fully_scroll printed_interpreter.screen } in

  (*
  Next, lexical analysis is performed on the text (except that in Versions 5 and later, if parsebuffer
  is zero then this is omitted). Initially, byte 0 of the parse-buffer should hold the maximum
  number of textual words which can be parsed. (If this is n, the buffer must be at least 2 +
  4*n bytes long to hold the results of the analysis.)
  *)

  let maximum_parse = read_byte new_screen_interpreter.story parse_address in

  if maximum_parse < 1 then failwith "bad parse buffer in sread";

  (*

  The interpreter divides the text into words and looks them up in the dictionary

  The number of words is written in byte 1 and one 4-byte block is written for each word, from
  byte 2 onwards (except that it should stop before going beyond the maximum number of words
  specified).

  Each block consists of the byte address of the word in the dictionary, if it is in the
  dictionary, or 0 if it isn't; followed by a byte giving the number of letters in the word; and finally
  a byte giving the position in the text-buffer of the first letter of the word.

  In Version 5 and later, this is a store instruction: the return value is the terminating character
  (note that the user pressing his "enter" key may cause either 10 or 13 to be returned; the author
  recommends that interpreters return 10).

  A timed-out input returns 0.

  Versions 1 and 2 and early Version 3 games mistakenly write the parse buffer length 240 into
  byte 0 of the parse buffer: later games fix this bug and write 59, because 2+4*59 = 238 so that 59
  is the maximum number of textual words which can be parsed into a buffer of length 240 bytes.
  Old versions of the Inform 5 library commit the same error. Neither mistake has very serious
  consequences.

  *)

  let tokens = tokenise trimmed new_screen_interpreter in

  let rec write_tokens items address count writing_tokens_interpreter =
    match items with
    | [] -> (count, writing_tokens_interpreter)
    | (tok, text_offset, dictionary_address) :: tail ->
      if count = maximum_parse then
        (count, writing_tokens_interpreter)
      else
        let write_story = writing_tokens_interpreter.story in
        let addr_story = write_word write_story address dictionary_address in
        let len_story =
          write_byte addr_story (address + 2) (String.length tok) in
        let offset_story =
          write_byte len_story (address + 3) (text_offset + 1) in
        let new_interpreter =
          { writing_tokens_interpreter with story = offset_story } in
        write_tokens tail (address + 4) (count + 1) new_interpreter in

  let (count, tokens_written_interpreter) =
    write_tokens tokens (parse_address + 2) 0 new_screen_interpreter in
  (* TODO: Make a write byte that takes interpreters *)
  let length_copied_story =
    write_byte tokens_written_interpreter.story (parse_address + 1) count in
  let length_copied_interpreter =
    { tokens_written_interpreter with story = length_copied_story } in
  handle_store_and_branch length_copied_interpreter instruction 0
  (* End of complete_sread *)

let handle_read_char interpreter instruction =
  (* TODO: Support for time routine *)
  { interpreter with
      state = Waiting_for_input ;
      input_max = 1 }

let complete_read_char interpreter instruction input =
    (*  TODO: Handle arguments; could require taking stuff off stack. *)
    let running_interpreter = { interpreter with state = Running } in
    handle_store_and_branch running_interpreter instruction (int_of_char input)

let handle_sread interpreter instruction =
  (* TODO: Variadic instruction *)

  (* This instruction is broken up into two halves. The first determines the size of
  the text buffer needed and then gives back an interpreter set to "I need input".
  The second half (above) does the actual work once the host has provided the data.

  Note that we are doing something unusual here. We potentially pop two values
  off the stack, but we discard the mutated interpreter state. We will simply
  compute the values again in the original interpreter on the completion side
  of the instruction! Immutable data structures for the win! *)

  let (text_address, _) =
    match instruction.operands with
    | [x_operand; y_operand] -> read_operand interpreter x_operand
    | _ -> failwith (Printf.sprintf "instruction at %04x must have two operands" instruction.address ) in

  (* SPEC
  This opcode reads a whole command from the keyboard (no prompt is automatically displayed).
  It is legal for this to be called with the cursor at any position on any window.
  In Versions 1 to 3, the status line is automatically redisplayed first.
  *)

  let status_interpreter = set_status_line interpreter in

  (* SPEC

  A sequence of characters is read in from the current input stream until a carriage return (or, in
  Versions 5 and later, any terminating character) is found.

  In Versions 1 to 4, byte 0 of the text-buffer should initially contain the maximum number of
  letters which can be typed, minus 1 (the interpreter should not accept more than this).

  The text typed is reduced to lower case (so that it can tidily be printed back by the program if need be)
  and stored in bytes 1 onward, with a zero terminator (but without any other terminator, such as a
  carriage return code). (This means that if byte 0 contains n then the buffer must contain n+1
  bytes, which makes it a string array of length n in Inform terminology.)
  *)

  let maximum_letters = read_byte status_interpreter.story text_address in

  (*
  Interpreters are asked to halt with a suitable error message if the text or parse buffers have
  length of less than 3 or 6 bytes, respectively: this sometimes occurs due to a previous array being
  overrun, causing bugs which are very difficult to find.
  *)

  if maximum_letters < 3 then failwith "bad text buffer in sread";

  (* TODO: At this point set the state to "needs input" and return that interpreter.
  The host will get the input and call back to complete the process. *)

  { status_interpreter with
      state = Waiting_for_input ;
      input_max = maximum_letters }
  (* end handle_sread *)

let display_interpreter interpreter =
  let pc = interpreter.program_counter in
  let frames = Frameset.display_frames interpreter.frames in
  let instr = display_instructions interpreter.story interpreter.program_counter 1 in
  Printf.sprintf "\nPC:%04x\n%s\n%s\n" pc frames instr

(* Move the interpreter on to the next instruction *)
let step_instruction interpreter =
  let instruction =
    decode_instruction interpreter.story interpreter.program_counter in

  (* Some helper routines for generic instructions that simply evaluate operands,
     compute a result from them, store, and branch. *)

  let handle_op0 compute_result =
    let (result, result_interpreter) = compute_result interpreter in
    handle_store_and_branch result_interpreter instruction result in

  let handle_op0_effect compute_effect =
    handle_op0 (fun i -> (0, compute_effect i)) in

  let handle_op0_value compute_value =
    handle_op0 (fun i -> (compute_value i, i)) in

  let handle_op1 compute_result =
    match instruction.operands with
    | [x_operand] ->
      let (x, operand_interpreter) = read_operand interpreter x_operand in
      let (result, result_interpreter) = compute_result x operand_interpreter in
      handle_store_and_branch result_interpreter instruction result
   | _ -> failwith (Printf.sprintf "instruction %s must have one operand" (Instruction.display instruction (version interpreter.story) ) ) in

  let handle_op1_effect compute_effect =
    handle_op1 (fun x i -> (0, compute_effect x i)) in

  let handle_op1_value compute_value =
    handle_op1 (fun x i -> (compute_value x i, i)) in

  let handle_op2 compute_result =
    match instruction.operands with
    | [x_operand; y_operand] ->
      let (x, x_interpreter) = read_operand interpreter x_operand in
      let (y, y_interpreter) = read_operand x_interpreter y_operand in
      let (result, result_interpreter) = compute_result x y y_interpreter in
      handle_store_and_branch result_interpreter instruction result
   | _ -> failwith (Printf.sprintf "instruction at %04x must have two operands" instruction.address ) in

  let handle_op2_effect compute_effect =
    handle_op2 (fun x y i -> (0, compute_effect x y i)) in

  let handle_op3 compute_result =
    match instruction.operands with
    | [x_operand; y_operand; z_operand] ->
      let (x, x_interpreter) = read_operand interpreter x_operand in
      let (y, y_interpreter) = read_operand x_interpreter y_operand in
      let (z, z_interpreter) = read_operand y_interpreter z_operand in
      let (result, result_interpreter) = compute_result x y z z_interpreter in
      handle_store_and_branch result_interpreter instruction result
    | _ -> failwith (Printf.sprintf "instruction at %04x must have three operands" instruction.address ) in

  let handle_op3_effect compute_effect =
    handle_op3 (fun x y z i -> (0, compute_effect x y z i)) in

  let handle_op3_value compute_value =
    handle_op3 (fun x y z i -> (compute_value x y z i, i)) in

  let handle_op4 compute_result =
    match instruction.operands with
    | [w_operand; x_operand; y_operand; z_operand] ->
      let (w, w_interpreter) = read_operand interpreter w_operand in
      let (x, x_interpreter) = read_operand w_interpreter x_operand in
      let (y, y_interpreter) = read_operand x_interpreter y_operand in
      let (z, z_interpreter) = read_operand y_interpreter z_operand in
      let (result, result_interpreter) = compute_result w x y z z_interpreter in
      handle_store_and_branch result_interpreter instruction result
     | _ -> failwith (Printf.sprintf "instruction at %04x must have four operands" instruction.address ) in

  let handle_op4_effect compute_effect =
    handle_op4 (fun w x y z i -> (0, compute_effect w x y z i)) in




  let handle_print_paddr paddr interp =
    let addr = decode_string_packed_address interp.story paddr in
    let text = read_zstring interp.story addr in
    interpreter_print interp text in

  let handle_load variable_number interpreter =
    (* The value of the operand is a number which represents the variable
    to be read from. So, for example, if the operand is sp then the
    value of the operand is the top of the stack, and the top of the stack
    contains a number. That number is then interpreted as a variable, and
    the value read from *that* variable is the result of the load. *)
    let variable = decode_variable variable_number in
    read_variable interpreter variable in


  let handle_nop interp =
    interp in

  let handle_split_window lines interp =
    (* TODO: in version 3 only, clear the upper window after the split. *)
    { interp with screen = split_window interp.screen lines } in

  let handle_restart () =
    (* If transcripting is active, this has to stay on in
    the restarted interpreter *)
    (* TODO: windowed screens might need work here *)
    let transcript_on = interpreter.transcript_selected in
    let transcript = interpreter.transcript in
    let commands = interpreter.commands in
    let story = original interpreter.story in
    let original = make story interpreter.screen in
    let restarted_interpreter = select_output_stream original TranscriptStream transcript_on in
    { restarted_interpreter with transcript = transcript; commands = commands } in

  let filename = "FLATHEAD.SAV" in
  (* TODO let save_failed = 0 in *)
  let save_succeeded = 1 in
  let restore_succeeded = 2 in

  let handle_save interp =
    let compressed = compress interp.story in
    let frames = Frameset.make_frameset_record interp.frames in

    (* TODO: The PC at present points to the save instruction. The convention,
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

    let root_form =
      Record [
        Header "FORM";
        Length None; (* The writer will figure it out *)
        SubHeader "IFZS";
        UnorderedList [
          Record [
            Header "IFhd";
            Length None;
            Integer16 (Some (release_number interp.story));
            ByteString (Some (serial_number interp.story), 6);
            Integer16 (Some (header_checksum interp.story));
            Integer24 (Some (interp.program_counter + 1)) ];
          Record [
            Header "CMem";
            Length None;
            RemainingBytes (Some compressed)];
          Record [
            Header "Stks";
            Length None;
            UnsizedList frames] ] ] in

    write_iff_file filename root_form;
    (* TODO: handle failure *)
    save_succeeded in (* end of handle_save *)

  let handle_restore () =
    (* TODO: This helper method can go into the IFF library *)
    let rec find_record chunks target =
      match chunks with
      | [] -> None
      | (Record (Header header :: _) as record) :: tail ->
        if header = target then Some record
        else find_record tail target
      | _ -> failwith "TODO: Handle failure in find_record" in

    (* In versions 1, 2 and 3 the restore branches on failure. In version 4 the
    restore stores a value on failure. Of course if the restore succeeds then
    the interpreter continues with the restored instruction pointer. *)

    (* TODO: Handle exceptions *)
    (* TODO: prompt for a filename *)

    let ifzd = read_iff_file filename ifzd_form in
    let chunks = match ifzd with
    | Record [
        Header "FORM";
        Length _;
        SubHeader "IFZS";
        UnorderedList items] ->
        items
    | _ -> failwith "TODO: Handle failure reading ifzs" in

    let ifhd_chunk = find_record chunks "IFhd" in
    let stacks_chunk = find_record chunks "Stks" in
    let umem_chunk = find_record chunks "UMem" in
    let cmem_chunk = find_record chunks "CMem" in

    let (release_number, serial_number, checksum, program_counter) =
      match ifhd_chunk with
      | Some (
          Record [
            Header "IFhd";
            Length _;
            Integer16 (Some release_number);
            ByteString (Some serial_number, 6);
            Integer16 (Some checksum);
            Integer24 (Some pc) ] ) ->
        (release_number, serial_number, checksum, pc)
      | _ -> failwith "TODO handle failure reading ifhd" in

    (* TODO: Check the release, serial number, checksum *)

    (* TODO: Move this logic into the frameset *)
    let frame_records =
      match stacks_chunk with
      | Some (
        Record [
          Header "Stks";
          Length _;
          UnsizedList items ] ) -> items
      | _ -> failwith "TODO handle failure reading stacks" in

    (* TODO: Deal with memory size mismatch. *)
    let frames = Frameset.make_frameset_from_records frame_records in

    (* TODO: Move this logic into the story *)
    let new_story =
      match (umem_chunk, cmem_chunk) with
      | (Some (
          Record [
            Header "UMem";
            Length (Some length);
            RemainingBytes Some bytes]),
        _) ->
          Story.apply_uncompressed_changes interpreter.story bytes
      | (_,
        Some (
          Record [
            Header "CMem";
            Length Some length;
            RemainingBytes Some bytes])) ->
        Story.apply_compressed_changes interpreter.story bytes
      | _ -> failwith "TODO handle failure reading memory" in

    (* TODO: If restore failed then we need to complete the restore instruction
    with result 0. *)

    (* If the restore succeeded then we need to complete the save instruction
      with result 2. See comments in handle_save that describe what is going
      on here. *)

    let save_pc = program_counter - 1 in
    let save_instruction = decode_instruction new_story save_pc in
    let new_interpreter = { interpreter with
      story = new_story;
      program_counter = save_pc;
      frames } in
    (* TODO: All the bits that have to be preserved *)

    (* After a restore, redraw the status line *)
    (* After a restore, collapse the upper window *)
    let new_interpreter = handle_split_window 0 new_interpreter in
    let new_interpreter = set_status_line new_interpreter in

    handle_store_and_branch new_interpreter save_instruction restore_succeeded in
  (* end of handle_restore *)

  let handle_storew arr ind value interp =
    { interp with story = write_word interp.story (arr + ind * 2) value } in

  let handle_storeb arr ind value interp =
    { interp with story = write_byte interp.story (arr + ind) value } in

  let handle_putprop obj prop value interp =
    { interp with story = write_property interp.story (Object obj) (Property prop) value } in

  let handle_print_char x interp =
    interpreter_print interp (Printf.sprintf "%c" (char_of_int x)) in

  let handle_print_num x interp =
    interpreter_print interp (Printf.sprintf "%d" (signed_word x)) in

  let handle_push x interp =
    push_stack interp x in

  let handle_output_stream_1 stream interp =
    let stream = signed_word stream in
    let new_interpreter = match stream with
    | 0 -> interp
    | 1 -> select_output_stream interp ScreenStream true
    | -1 -> select_output_stream interp ScreenStream false
    | 2 -> select_output_stream interp TranscriptStream true
    | -2 -> select_output_stream interp TranscriptStream true
    | 3 -> failwith "Illegal to select stream 3 without table "
    | -3 -> deselect_memory_stream interp
    | 4 -> select_output_stream interp CommandStream true
    | -4 -> select_output_stream interp CommandStream true
    | _ -> failwith (Printf.sprintf "Invalid stream %d in output_stream" stream) in
    new_interpreter in

  let handle_output_stream_2 stream table interp =
    if stream = 3 then
      select_memory_stream interp table
    else
      handle_output_stream_1 stream interp in

  let handle_output_stream () =
    match instruction.operands with
    | [_] -> handle_op1_effect handle_output_stream_1
    | [_; _] -> handle_op2_effect handle_output_stream_2
    | _ -> failwith "output_stream requires 1 or 2 arguments" in

  let handle_input_stream stream interpreter =
    (* TODO: input_stream not yet implemented; treat as a no-op for now. *)
    interpreter in

  let handle_sound_effect() =
    (* TODO: sound_effect not yet implemented; treat as a no-op for now. *)
    interpreter in

  let handle_set_window window interpreter =
    let w =
      match window with
      | 0 -> Lower_window
      | 1 -> Upper_window
      | _ -> failwith "Unexpected window in set_window" in
    { interpreter with screen = set_window interpreter.screen w } in

  let handle_erase_line value interpreter =

  (* Spec:
  Versions 4 and 5: if the value is 1, erase from the current cursor
  position to the end of its line in the current window. If the value
  is anything other than 1, do nothing. *)

  if value = 1 then
    { interpreter with screen = erase_line interpreter.screen }
  else
    interpreter in

  let handle_erase_window window interp =
    (* Spec:

      Erases window with given number (to background colour); or
      if -1 it unsplits the screen and clears the lot; or if -2 it clears
      the screen without unsplitting it. In cases -1 and -2, the cursor may
      move *)

    (* In Versions 5 and later, the cursor for the window being erased should
      be moved to the top left. *)

    (* In Version 4, the lower window's cursor moves to its bottom left,
       while the upper window's cursor moves to top left *)
    let window = signed_word window in
    let unsplit = match window with
      | -2 -> interp.screen
      | -1 -> split_window interp.screen 0
      | _ -> interp.screen in
    let erased = match window with
      | -2
      | -1 -> erase_all unsplit
      | 0 -> erase_lower unsplit
      | 1 -> erase_upper unsplit
      | _ -> failwith "unexpected window number in erase_window" in
    let upper_moved = match window with
      | -2
      | -1
      | 1 -> set_upper_cursor erased 1 1
      | _ -> erased in
    let lower_moved = match window with
      | -2
      | -1
      | 0 ->
        if (version interp.story) <= 4 then
          set_lower_cursor upper_moved 1 (upper_moved.height)
        else
          set_lower_cursor upper_moved 1 1
      | _ -> upper_moved in
    { interp with screen = lower_moved } in

  let handle_set_cursor line column interp =
    (* Spec 8.7.2.3
    When the upper window is selected, its cursor position can be moved with
    set_cursor. The opcode has no effect when the lower window is selected.
    It is illegal to move the cursor outside the current size of the upper
    window. *)
    match interp.screen.selected_window with
    | Lower_window -> interp
    | Upper_window ->
      { interp with screen = set_cursor interp.screen column line } in

  let handle_get_cursor array interpreter =
    (* Spec:
    Puts the current cursor row into the word 0 of the given array, and the
    current cursor column into word 1. (The array is not a table and has no
    size information in its initial entry.) *)
    let (x, y) = get_active_cursor interpreter.screen in
    let story1 = write_word interpreter.story array y in
    let story2 = write_word story1 (array + 2) x in
    { interpreter with story = story2 } in

  let handle_buffer_mode flag interpreter =
    (* Spec:
    If set to 1, text output on the lower window in stream 1 is buffered up
    so that it can be word wrapped properly. If set to 0, it isn't. *)
    (* I note that this code implements the spec; did the spec intend to leave
    unspecified what happens when the value is neither 0 nor 1? *)
    match flag with
    | 0 -> { interpreter with screen =
      set_word_wrap interpreter.screen Word_wrap_disabled }
    | 1 -> { interpreter with screen =
      set_word_wrap interpreter.screen Word_wrap_enabled }
    | _ -> interpreter in

  let handle_set_text_style style interpreter =
    (* TODO: set_text_style not yet implemented; treat as a no-op for now. *)
    interpreter in

  let handle_tokenise () =
    failwith "tokenise not implemented" in

  let handle_encode_text () =
    failwith "TODO encode_text not implemented" in

  let handle_copy_table first second size interpreter =
    (* Spec:
    If second is zero, then size bytes of first are zeroed. Otherwise first
    is copied into second, its length in bytes being the absolute value of
    size (i.e., size if size is positive, -size if size is negative).
    The tables are allowed to overlap. If size is positive, the interpreter
    must copy either forwards or backwards so as to avoid corrupting first in
    the copying process. If size is negative, the interpreter
    must copy forwards even if this corrupts first. *)
    failwith "TODO copy_table not implemented" in

  let handle_print_table text width height skip interpreter =
  (* TODO: Note: variadic instruction *)
      (* Spec:
      Print a rectangle of text on screen spreading right and down from the
      current cursor position, of given width and height, from the table of
      ZSCII text given. (Height is optional and defaults to 1.) If a skip
      value is given, then that many characters of text are skipped over in
      between each line and the next. (So one could make this display, for
      instance, a 2 by 3 window onto a giant 40 by 40 character graphics map.) *)

      failwith "TODO print_table not implemented" in

  let handle_check_arg_count number interpreter =
    (* Spec:
      Branches if the given argument-number (counting from 1) has been
      provided by the routine call to the current routine. (This allows
      routines in Versions 5 and later to distinguish between the calls
      routine(1) and routine(1,0), which would otherwise be impossible to
      tell apart.) *)

    failwith "TODO check_arg_count not implemented" in

  let handle_pop interpreter =
    pop_stack interpreter in

  let handle_new_line interp =
    interpreter_print interp "\n" in

  let handle_show_status interp =
    set_status_line interp in

  let handle_verify interp =
    if verify_checksum interp.story then 1 else 0 in

  let handle_piracy interp =
    1 in

  let handle_print interp =
    match instruction.text with
    | Some text -> interpreter_print interp text
    | _ -> failwith "no text in print instruction" in


  let handle_random n interpreter =
    if n = 0 then
      (0, { interpreter with random = Randomness.make_random()})
    else if n < 0 then
      (0, { interpreter with random = Randomness.make_seeded n })
    else
      let (result, random) = Randomness.next interpreter.random n in
      (result, { interpreter with random }) in

  (* Some helpers for instructions that are a bit unusual, like returns *)

  (* For an unconditional jump we might as well just evaluate the operand and branch directly. *)
  let handle_jump () =
    match instruction.operands with
    | [target_operand] ->
      let (relative_target, target_interpreter) = read_operand interpreter target_operand in
      let absolute_target = instruction.address + instruction.length + (signed_word relative_target) - 2 in
      set_program_counter target_interpreter absolute_target
    | _ -> failwith "instruction must have one operand" in


  (* Do not advance to the next instruction *)
  let handle_quit () =
    { interpreter with state = Halted } in

  let handle_print_ret () =
    let printed_interpreter =
      match instruction.text with
      | Some text -> interpreter_print interpreter (text ^ "\n")
      | _ -> failwith "no text in print_ret instruction" in
    interpret_return printed_interpreter instruction 1 in

  let handle_ret_popped () =
    interpret_return (pop_stack interpreter) instruction (peek_stack interpreter) in

  let handle_ret () =
    match instruction.operands with
    | [lone_operand] ->
      let (result, operand_interpreter) = read_operand interpreter lone_operand in
      interpret_return operand_interpreter instruction result
    | _ -> failwith "instruction must have one operand" in

  let handle_rtrue () =
    interpret_return interpreter instruction 1 in

  let handle_rfalse () =
    interpret_return interpreter instruction 0 in

  let handle_scan_table x table len interp =
    (* TODO: This is variadic; also has a 4-argument version *)
    (* Does word x occur in table of len words? If yes, give
    the address. If no, zero. *)
    let rec aux i =
      if i = len then
        0
      else
        let addr = table + 2 * i in
        let y = unsigned_word (read_word interp.story addr) in
        if x = y then addr
        else aux (i + 1) in
    aux 0 in

  let handle_catch() =
    failwith "catch instruction TODO" in






  let (arguments, arguments_interp) = operands_to_arguments interpreter instruction.operands in
  let interpret_instruction = interpret_instruction arguments_interp instruction in
  let value = interpret_value_instruction arguments_interp instruction in
  let effect = interpret_effect_instruction arguments_interp instruction in

  match (instruction.opcode, arguments) with
  | (ILLEGAL, _) ->  failwith "illegal operand"
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
  | (OP2_25, [routine; arg1]) -> handle_call routine [arg1] arguments_interp instruction
  | (OP2_26, [routine; arg1]) -> handle_call routine [arg1] arguments_interp instruction
  | (OP2_27, [foreground; background]) -> effect (handle_set_colour2 foreground background)
  | (OP2_27, [foreground; background; window]) -> effect (handle_set_colour3 foreground background window)
  | (OP2_28, [x; frame]) -> handle_throw x frame arguments_interp
  | (OP1_128, [a]) -> value (handle_jz a)
  | (OP1_129, [obj]) -> value (handle_get_sibling obj)
  | (OP1_130, [obj]) -> value (handle_get_child obj)
  | (OP1_131, [obj]) -> value (handle_get_parent obj)
  | (OP1_132, [property_address]) -> value (handle_get_prop_len property_address)
  | (OP1_133, [variable]) -> effect (handle_inc variable)
  | (OP1_134, [variable]) -> effect (handle_dec variable)
  | (OP1_135, [address]) -> effect (handle_print_addr address)
  | (OP1_136, [routine]) -> handle_call routine [] arguments_interp instruction
  | (OP1_137, [obj]) -> effect (handle_remove_obj obj)
  | (OP1_138, [obj]) -> effect (handle_print_obj obj)


  | (OP1_143, [x]) ->
    if (version interpreter.story) <= 4 then
      value (handle_not x)
    else
      handle_call x [] arguments_interp instruction

  | (VAR_224, routine :: args) -> handle_call routine args arguments_interp instruction
  | (VAR_236, routine :: args) -> handle_call routine args arguments_interp instruction

  | (VAR_248, [x]) -> value (handle_not x)
  | (VAR_249, routine :: args) -> handle_call routine args arguments_interp instruction
  | (VAR_250, routine :: args) -> handle_call routine args arguments_interp instruction

  | _ ->

(
  match instruction.opcode with

  | OP1_139 -> handle_ret ()
  | OP1_140 -> handle_jump ()
  | OP1_141 -> handle_op1_effect handle_print_paddr
  | OP1_142 -> handle_op1 handle_load
  | OP0_176 -> handle_rtrue ()
  | OP0_177 -> handle_rfalse ()
  | OP0_178 -> handle_op0_effect handle_print
  | OP0_179 -> handle_print_ret ()
  | OP0_180 -> handle_op0_effect handle_nop
  | OP0_181 -> handle_op0_value handle_save
  | OP0_182 -> handle_restore ()
  | OP0_183 -> handle_restart ()
  | OP0_184 -> handle_ret_popped ()
  | OP0_185 ->
    if (version interpreter.story <= 4) then
      handle_op0_effect handle_pop
    else
      handle_catch()
  | OP0_186 -> handle_quit ()
  | OP0_187 -> handle_op0_effect handle_new_line
  | OP0_188 -> handle_op0_effect handle_show_status
  | OP0_189 -> handle_op0_value handle_verify
  | OP0_190 -> failwith "190 is the extended opcode marker"
  | OP0_191 -> handle_op0_value handle_piracy

  | VAR_225 -> handle_op3_effect handle_storew
  | VAR_226 -> handle_op3_effect handle_storeb
  | VAR_227 -> handle_op3_effect handle_putprop
  | VAR_228 -> handle_sread interpreter instruction
  | VAR_229 -> handle_op1_effect handle_print_char
  | VAR_230 -> handle_op1_effect handle_print_num
  | VAR_231 -> handle_op1 handle_random
  | VAR_232 -> handle_op1_effect handle_push
  | VAR_233 -> handle_pull interpreter instruction
  | VAR_234 -> handle_op1_effect handle_split_window
  | VAR_235 -> handle_op1_effect handle_set_window
  | VAR_237 -> handle_op1_effect handle_erase_window
  | VAR_238 -> handle_op1_effect handle_erase_line
  | VAR_239 -> handle_op2_effect handle_set_cursor
  | VAR_240 -> handle_op1_effect handle_get_cursor
  | VAR_241 -> handle_op1_effect handle_set_text_style
  | VAR_242 -> handle_op1_effect handle_buffer_mode
  | VAR_243 -> handle_output_stream ()
  | VAR_244 -> handle_op1_effect handle_input_stream
  | VAR_245 -> handle_sound_effect()
  | VAR_246 -> handle_read_char interpreter instruction
  | VAR_247 -> handle_op3_value handle_scan_table

  | VAR_251 -> handle_tokenise()
  | VAR_252 -> handle_encode_text()
  | VAR_253 -> handle_op3_effect handle_copy_table
  | VAR_254 -> handle_op4_effect handle_print_table
  | VAR_255 -> handle_op1_value handle_check_arg_count
  | EXT_0   -> failwith (Printf.sprintf "%04x TODO: EXT_0" instruction.address)
  | EXT_1   -> failwith (Printf.sprintf "%04x TODO: EXT_1" instruction.address)
  | EXT_2   -> failwith (Printf.sprintf "%04x TODO: EXT_2" instruction.address)
  | EXT_3   -> failwith (Printf.sprintf "%04x TODO: EXT_3" instruction.address)
  | EXT_4   -> failwith (Printf.sprintf "%04x TODO: EXT_4" instruction.address)
  | EXT_5   -> failwith (Printf.sprintf "%04x TODO: EXT_5" instruction.address)
  | EXT_6   -> failwith (Printf.sprintf "%04x TODO: EXT_6" instruction.address)
  | EXT_7   -> failwith (Printf.sprintf "%04x TODO: EXT_7" instruction.address)
  | EXT_8   -> failwith (Printf.sprintf "%04x TODO: EXT_8" instruction.address)
  | EXT_9   -> failwith (Printf.sprintf "%04x TODO: EXT_9" instruction.address)
  | EXT_10   -> failwith (Printf.sprintf "%04x TODO: EXT_10" instruction.address)
  | EXT_11   -> failwith (Printf.sprintf "%04x TODO: EXT_11" instruction.address)
  | EXT_12   -> failwith (Printf.sprintf "%04x TODO: EXT_12" instruction.address)
  | EXT_13   -> failwith (Printf.sprintf "%04x TODO: EXT_13" instruction.address)
  | EXT_14   -> failwith (Printf.sprintf "%04x TODO: EXT_14" instruction.address)
  | EXT_16   -> failwith (Printf.sprintf "%04x TODO: EXT_16" instruction.address)
  | EXT_17   -> failwith (Printf.sprintf "%04x TODO: EXT_17" instruction.address)
  | EXT_18   -> failwith (Printf.sprintf "%04x TODO: EXT_18" instruction.address)
  | EXT_19   -> failwith (Printf.sprintf "%04x TODO: EXT_19" instruction.address)
  | EXT_20   -> failwith (Printf.sprintf "%04x TODO: EXT_20" instruction.address)
  | EXT_21   -> failwith (Printf.sprintf "%04x TODO: EXT_21" instruction.address)
  | EXT_22   -> failwith (Printf.sprintf "%04x TODO: EXT_22" instruction.address)
  | EXT_23   -> failwith (Printf.sprintf "%04x TODO: EXT_23" instruction.address)
  | EXT_24   -> failwith (Printf.sprintf "%04x TODO: EXT_24" instruction.address)
  | EXT_25   -> failwith (Printf.sprintf "%04x TODO: EXT_25" instruction.address)
  | EXT_26   -> failwith (Printf.sprintf "%04x TODO: EXT_26" instruction.address)
  | EXT_27   -> failwith (Printf.sprintf "%04x TODO: EXT_27" instruction.address)
  | EXT_28   -> failwith (Printf.sprintf "%04x TODO: EXT_28" instruction.address)
  | EXT_29   -> failwith (Printf.sprintf "%04x TODO: EXT_29" instruction.address)
  | _ -> failwith "should be impossible to get here"
)
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
    if needs_more interpreter.screen then
      clear_more interpreter.screen
    else
      interpreter.screen in
  if needs_scroll screen then
    { interpreter with screen = scroll screen; has_new_output = true }
  else
    step_instruction { interpreter with screen; has_new_output = false }

let step_with_input interpreter key =
  let key_text = string_of_char key in
  let length = String.length interpreter.input in
  let instruction =
    decode_instruction interpreter.story interpreter.program_counter in
  let handle_enter () =
    let blank_input = { interpreter with input = "" } in
    complete_sread blank_input instruction interpreter.input in
  let handle_backspace () =
    if length = 0 then
      interpreter
    else
      { interpreter with input = truncate interpreter.input (length - 1)} in
  match instruction.opcode with
  | VAR_246 -> complete_read_char interpreter instruction key
  | VAR_228 ->
    if key_text = "\r" then handle_enter()
    else if key_text = "\b" then handle_backspace()
    else if length >= interpreter.input_max then interpreter
    else { interpreter with input = interpreter.input ^ key_text }
  | _ -> failwith "not waiting for input"
