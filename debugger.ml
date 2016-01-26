open Utility
open Graphics
open Screen
open Instruction
open Interpreter;;

open_graph "";;
auto_synchronize false;;
set_font "Lucida Console";;
let (text_width, text_height) = text_size "X";;

let draw_string_at text x y =
  Graphics.moveto x y;
  Graphics.draw_string text;;

type state =
  | Paused
  | Running
  | Halted
  | Stepping of int

type action =
  | Pause
  | StepBackwards
  | StepForwards
  | Run
  | Quit
  | Keystroke of char
  | NoAction

type t =
{
  undo_stack : Interpreter.t list;
  redo_stack : Interpreter.t list;
  interpreter : Interpreter.t;
  state : state;
  keystrokes : string;
  buttons :  (Button.t * action) list;
}

(* Extra line for status *)
let screen_extent screen =
  (10, 10, screen.width * text_width, (screen.height + 1) * text_height);;

let make interpreter =
  let (x, y, _, h) = screen_extent interpreter.screen in
  let margin = 20 in
  let gap = 10 in
  let button_y = y + h + gap in
  let button_list =
    [
      ("X", Quit);
      ("<|", StepBackwards);
      ("||", Pause);
      (">", Run);
      ("|>", StepForwards)
    ] in
    let button_map =
      let rec aux map buttons button_x =
        match buttons with
        | [] -> map
        | (caption, action) :: tail ->
          let new_button = Button.make button_x button_y margin caption in
          let new_x = button_x + new_button.Button.width + gap in
          aux ((new_button, action) :: map) tail new_x in
      aux [] button_list x in
  {
    undo_stack = [];
    redo_stack = [];
    interpreter = interpreter;
    state = Running;
    keystrokes = "";
    buttons = button_map
  }

let clear_screen screen =
  let (x, y, w, h) = screen_extent screen in
  set_color background;
  fill_rect x y w h

let draw_status screen =
  let (x, y, _, _) = screen_extent screen in
  let status_color = blue in
  match screen.status with
  | Status None  -> ()
  | Status Some status -> (
    set_color status_color;
    draw_string_at status x (y + text_height * screen.height) )

let rec draw_screen screen =
  clear_screen screen;
  draw_status screen;
  set_color foreground;
  let (x, y, _, _) = screen_extent screen in
  let rec aux n =
    if n < screen.height then (
      let text = Deque.peek_front_at (screen_lines screen) n in
      let text_y = y + text_height * n in
      draw_string_at text x text_y;
      aux (n + 1)) in
  aux 0 ;
  synchronize()

let trim_to_length text length =
  if (String.length text) <= length then text
  else String.sub text 0 length

(* x and y in screen coordinates; width and height in characters *)
let draw_before_current_after before current after x y width height =
  let before_color = blue in
  let current_color = black in
  let after_color = blue in
  let rec draw_before items n =
    if n < height then
      match items with
      | [] -> ()
      | text :: tail -> (
        draw_string_at text x (y + n * text_height);
        draw_before tail (n + 1)) in
  let rec draw_after items n =
    if n > 0 then
      match items with
      | [] -> ()
      | text :: tail -> (
        draw_string_at text x (y + n * text_height);
        draw_after tail (n - 1)) in
  set_color background;
  fill_rect x y (width * text_width) (height * text_height);
  set_color before_color;
  draw_before before (height / 2 + 1);
  set_color current_color;
  draw_string_at current x (y + text_height * (height / 2));
  set_color after_color;
  draw_after after (height / 2 - 1);
  set_color foreground;
  synchronize();;

(* TODO: use draw_before_current_after *)
let draw_undo_redo debugger =
  let undo_color = blue in
  let redo_color = blue in
  let current_color = black in
  let interpreter = debugger.interpreter in
  let (screen_x, screen_y, screen_w, screen_h) = screen_extent interpreter.screen in
  let window_x = screen_x + screen_w + 10 in
  let window_y = screen_y in
  let instruction_width = 60 in
  let window_w = text_width * instruction_width in
  let window_h = text_height * interpreter.screen.height in
  let draw_line interp n =
    let instr = (Story.display_instructions interp.story interp.program_counter 1) in
    let text = trim_to_length instr instruction_width in
    draw_string_at text window_x (window_y + text_height * n) in
  let rec draw_undo undo n =
    if n < interpreter.screen.height then
      match undo with
      | [] -> ()
      | h :: t -> (
        draw_line h n;
        draw_undo t (n + 1)) in
  let rec draw_redo redo n =
    if n > 0 then
      match redo with
      | [] -> ()
      | h :: t -> (
        draw_line h n;
        draw_redo t (n - 1)) in
  set_color background;
  fill_rect window_x window_y window_w window_h;
  set_color undo_color;
  draw_undo debugger.undo_stack (interpreter.screen.height / 2 + 1);
  set_color current_color;
  draw_line debugger.interpreter (interpreter.screen.height / 2);
  set_color redo_color;
  draw_redo debugger.redo_stack (interpreter.screen.height / 2 - 1);
  set_color foreground;
  synchronize()

let debugger_push_undo debugger new_interpreter =
  if new_interpreter.program_counter = debugger.interpreter.program_counter then
    { debugger with interpreter = new_interpreter; redo_stack = [] }
  else
    { debugger with interpreter = new_interpreter;
      undo_stack = debugger.interpreter :: debugger.undo_stack;
      redo_stack = [] }

let needs_more debugger =
  Screen.needs_more debugger.interpreter.screen

let has_keystrokes debugger =
  (String.length debugger.keystrokes) > 0

let draw_interpreter debugger =
  let interpreter = debugger.interpreter in
  let screen = interpreter.screen in
  if interpreter.state = Waiting_for_input then
    draw_screen (fully_scroll (Screen.print screen interpreter.input))
  else if interpreter.has_new_output || (debugger.state = Paused) || (debugger.state = Halted) then
    let screen_to_draw =
      if needs_more debugger then
        more screen
      else
        screen in
    draw_screen screen_to_draw

let step_reverse debugger =
  match debugger.undo_stack with
  | [] -> debugger
  | h :: t -> { debugger with
    undo_stack = t;
    interpreter = h;
    redo_stack = debugger.interpreter :: debugger.redo_stack };;

let step_forward debugger =
  match debugger.redo_stack with
  | h :: t -> { debugger with
    undo_stack = debugger.interpreter :: debugger.undo_stack;
    interpreter = h;
    redo_stack = t }
  | [] ->
    let interpreter = debugger.interpreter in
    match interpreter.state with
    | Interpreter.Waiting_for_input ->
      (* If we have pending keystrokes then take the first one off the queue
      and give it to the interpreter. Otherwise just put this on the undo
      stack and return to the caller otherwise unchanged. We can't progress
      until someone gives us a key. *)
      let (new_interpreter, new_keys) =
        if debugger.keystrokes = "" then
          (interpreter, debugger.keystrokes)
        else
          (step_with_input interpreter debugger.keystrokes.[0],
          String.sub debugger.keystrokes 1 ((String.length debugger.keystrokes) - 1)) in
      { (debugger_push_undo debugger new_interpreter) with keystrokes = new_keys }
    | Interpreter.Halted -> debugger (* TODO: Exception? *)
    | Interpreter.Running ->
      let new_interpreter = step interpreter in
      debugger_push_undo debugger new_interpreter;;

let waiting_for_input debugger =
  match debugger.interpreter.state with
  | Waiting_for_input -> true
  | _ -> false;;

let rec obtain_action debugger should_block =
  (* A keystroke observed with Poll is not removed from the queue
  of keystrokes! It will keep coming back every time we poll. We
  therefore only consider keystroke events as having happened
  when we are blocking while waiting for input. *)
  let events =
    if should_block then [Key_pressed; Button_down]
    else [Poll] in
  let status = wait_next_event events in
  if should_block && status.keypressed then
    Keystroke status.key
  else
    let action =
      let is_hit (button, _) =
        Button.was_clicked button status.mouse_x status.mouse_y in
      if status.button then
        match List.filter is_hit debugger.buttons with
        | [] -> NoAction
        | (_, action) :: _ -> action
      else
        NoAction in
    if action = NoAction && should_block then
      (* If we're blocking until something happens, do not report NoAction. *)
      obtain_action debugger should_block
    else
      action

let pause debugger =
  { debugger with state = Paused }

let start_running debugger =
  { debugger with state = Running }

let clear_redo debugger =
  { debugger with redo_stack = [] }

let add_keystroke debugger key =
 { debugger with keystrokes = debugger.keystrokes ^ (string_of_char key) }

let remove_keystroke debugger =
  let k = debugger.keystrokes in
  { debugger with keystrokes = String.sub k 1 ((String.length k) - 1) }

let set_step_instruction debugger instruction =
  { debugger with state = Stepping instruction }

let maybe_step debugger =
  let should_step =
    match debugger.state with
    | Running -> true
    | Stepping instruction -> debugger.interpreter.program_counter = instruction
    | _ -> false in
  if should_step then step_forward debugger
  else debugger

let halt debugger =
  { debugger with state = Halted }

  let draw_routine_listing debugger =
    let current_instruction = debugger.interpreter.program_counter in
    (* This can be zero if we were restored from a save game *)
    let frame_instruction = (current_frame debugger.interpreter).Frame.called in
    let first_instruction =
      if frame_instruction = 0 then current_instruction
      else frame_instruction in
    let story = debugger.interpreter.story in
    let current = Instruction.display  (Story.decode_instruction story current_instruction) (Story.version story) in
    let reachable = Story.all_reachable_addresses_in_routine story first_instruction in
    let sorted = List.sort compare reachable in
    let decode instr =
      (instr, Instruction.display (Story.decode_instruction story instr) (Story.version story)) in
    let map = List.map decode sorted in
    let rec aux before after map =
      match map with
      | [] -> (before, after)
      | (addr, text) :: tail ->
        if addr < current_instruction then aux (text :: before) after tail
        else if addr > current_instruction then aux before (text :: after) tail
        else aux before after tail in
    let (before, after) = aux [] [] map in
    let screen = debugger.interpreter.screen in
    let (screen_x, screen_y, screen_w, screen_h) = screen_extent screen in
    let x = screen_x + screen_w + 10 in
    draw_before_current_after before current (List.rev after) x screen_y 60 screen.height

(* TODO: Most of the methods in this module can be local to run *)

let run debugger =
  let rec main_loop debugger =
    (* Put the debugger into the right state, depending on the interpreter *)
    let debugger =
      match (debugger.state, debugger.interpreter.state) with
      | (_, Interpreter.Halted) -> halt debugger
      | (Stepping instruction, _) ->
        if debugger.interpreter.program_counter = instruction then debugger
        else pause debugger
      | _ -> debugger in

    (* Under what circumstances do we need to block?
       1 if the debugger is not running then block
       2 if the debugger is running, has no queued input, and is waiting for input, then block
       3 if the debugger is running, has no queued input, and is waiting for --MORE--, then block
    *)
    let running =
      match debugger.state with
      | Halted
      | Paused -> false
      | _ -> true in

    let needs_more = needs_more debugger in
    let waiting_for_input = waiting_for_input debugger in
    let has_keystrokes = has_keystrokes debugger in
    let should_block = (not running) || ((not has_keystrokes) && (waiting_for_input || needs_more)) in
    draw_interpreter debugger;
    if should_block then draw_routine_listing debugger;
    let action = obtain_action debugger should_block in
    match action with
    | Pause -> main_loop (pause debugger)
    | StepBackwards -> main_loop (pause (step_reverse debugger))
    | StepForwards -> main_loop (set_step_instruction debugger debugger.interpreter.program_counter)
    | Run -> main_loop (start_running (clear_redo debugger))
    | Quit -> ()
    | Keystroke key -> main_loop (add_keystroke debugger key)
    | NoAction ->
      (* Suppose we blocked because we had --MORE-- but no queued keystrokes.
      If we then got here we must have queued up a keystroke, which is still
      in the queue. The step will clear out the needs_more, but we need to
      lose that keystroke *)
      let new_debugger =
        if needs_more && has_keystrokes then remove_keystroke debugger
        else debugger in
      main_loop (maybe_step new_debugger) in
    List.iter (fun (b, _) -> Button.draw b) debugger.buttons;
    main_loop debugger
