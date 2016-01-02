open Graphics;;

open_graph "";;



let events = [Key_pressed];;

let string_of_char x = String.make 1 x;;

let text_max_height = 10;;
let text_max_width = 40;;

(* Takes a list of strings, the (x, y) coordinates of the bottom left
   corner of a window, the height of the window in lines, and the number of pixels
   per line. Pops the list until the window is full or the list is empty. *)
   
let display_lines lines x y max_lines line_height = 
    let rec aux lines n = 
        if n > max_lines then ()
        else
            match lines with
            | [] -> ()
            | line :: tail -> (
                moveto x (y + line_height * n);
                draw_string line;
                aux tail (n + 1) ) in
    aux lines 0;;

(* Takes a string, a character and an index; finds
the highest index that matches the character at or before
the given index. If there is no match then None is returned. *)

let rec reverse_index_from text target index =
    if index < 0 then None
    else if text.[index] = target then Some index
    else reverse_index_from text target (index - 1);;

(* Word-wraps the last line in a list of lines. Assumes that
the tail of the list is already word-wrapped. Returns the 
new list and the number of lines added to the given list. *)
    
let rec wrap_lines lines wrap_size =
    let rec aux lines count =
        match lines with
        | [] -> ([], 0)
        | h :: t -> 
            let len = String.length h in
            if String.contains h '\r' then
                (* Recursive case 1: there is a break in the last string.
                   Split the string, solve the wrapping problem with no return,
                   and then recurse on the remainder of the string. *)
                let b = String.index h '\r' in
                let f = String.sub h 0 b in
                let r = String.sub h (b + 1) (len - b - 1) in
                let (w1, c1) = wrap_lines (f :: t) wrap_size in
                let (w2, c2) = wrap_lines (r :: w1) wrap_size in
                (w2, c1 + c2 + 1) (* Note that count should be zero *)
            else if len > wrap_size then
                (* Recursive case 2: there are no breaks but the line is too long.
                   Find a space to break on, break it, and recurse. *)
                let space_location = reverse_index_from h ' ' wrap_size in
                let break_point = match space_location with
                    | None -> wrap_size
                    | Some location -> location in
                aux ((String.sub h (break_point + 1) (len - break_point - 1)) :: (String.sub h 0 break_point) :: t) (count + 1)
            else
                (* Base case: the line has no breaks and is short enough. Do nothing. *)
                (lines, count) in
    aux lines 0;;
            
let add_to_lines lines str =
    match lines with 
    | [] -> [str]
    | h :: t -> (h ^ str) :: t ;;
    
    
type instruction = 
    | Print of string
    | Newline
    | Prompt 
    | Quit;;
    
type interpreter_state =
    | Running
    | Halted
    | Printing_is_waiting_for_key
    | Prompting_is_waiting_for_string ;;
    
type interpreter =
{
    instructions : instruction list;
    state : interpreter_state;
    transcript : string list;
    lines_added_since_last_input : int;
    pending_lines : string list
};;

(* Pops up to n items from source, pushes onto destination,
returns popped source, pushed destination, and number of items 
actually moved. *)

let move source destination number =
    let rec aux s d n c = 
        if n = 0 then (s, d, c)
        else match s with
        | [] -> (s, d, c)
        | h :: t -> aux t (h :: d) (n - 1) (c + 1) in
    aux source destination number 0;;
    
let move_all source destination =
    let rec aux s d c = 
        match s with
        | [] -> (d, c)
        | h :: t -> aux t (h :: d) (c + 1) in
    aux source destination 0;;
    


let next_instruction interpreter =
    { interpreter with instructions = List.tl interpreter.instructions; state = Running };;

let try_to_complete_printing interpreter =

    (* If there are pending lines then we must have just gotten a keypress. 
    Move them into the transcript and note how many lines were added since the keypress. *)
    
    let (transcript_with_pending, moved_into_transcript) = move_all interpreter.pending_lines interpreter.transcript in
    let new_interpreter = if moved_into_transcript = 0 then interpreter 
    else { interpreter with 
        lines_added_since_last_input = moved_into_transcript;
        transcript = transcript_with_pending;
        pending_lines = [] } in

    if new_interpreter.lines_added_since_last_input <= text_max_height then 
        (* We can keep on partying *)
        next_instruction new_interpreter
    else 
        (* We are over-full. Move some of the lines into pending. Move one more than we need to 
        to make room for the --MORE-- line *)
        
        let (new_transcript, new_pending, moved_into_pending) = move new_interpreter.transcript [] (new_interpreter.lines_added_since_last_input - text_max_height + 1) in
        { new_interpreter with 
            transcript = new_transcript; 
            pending_lines = new_pending; 
            state = Printing_is_waiting_for_key;
            lines_added_since_last_input = new_interpreter.lines_added_since_last_input - moved_into_pending
        };;

let do_print interpreter s = 
    let (new_transcript, lines_added) = wrap_lines (add_to_lines interpreter.transcript s) text_max_width in
    try_to_complete_printing { interpreter with 
        transcript = new_transcript; 
        lines_added_since_last_input = interpreter.lines_added_since_last_input + lines_added };;
    
let do_newline interpreter =
    try_to_complete_printing { interpreter with 
        transcript = "" :: interpreter.transcript; 
        lines_added_since_last_input = interpreter.lines_added_since_last_input + 1 };;
  
let complete_prompting interpreter s =
    let (new_transcript, _) = wrap_lines (add_to_lines interpreter.transcript s) text_max_width in 
    next_instruction { interpreter with transcript = "" :: new_transcript ; lines_added_since_last_input = 0 };;
   
let step interpreter =
    match (interpreter.state, interpreter.instructions) with
    | (_, [])  
    | (Halted, _) -> failwith "halted"
    | (Printing_is_waiting_for_key, _) -> failwith "waiting for key" 
    | (Prompting_is_waiting_for_string, _) -> failwith "waiting for string"
    | (Running, (Print s) :: _) -> do_print interpreter s
    | (Running, Newline :: _) -> do_newline interpreter  
    | (Running, Prompt :: _) -> { interpreter with state = Prompting_is_waiting_for_string }
    | (Running, Quit :: _ ) -> { interpreter with state = Halted };;
    
let step_with_input interpreter input =
    match (interpreter.state, interpreter.instructions) with
    | (Printing_is_waiting_for_key, _) -> try_to_complete_printing interpreter
    | (Prompting_is_waiting_for_string, _) -> complete_prompting interpreter input
    | _ -> failwith "not expecting input"
    
let lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\r\rUt enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";;
let prompt = "> ";;

let main_interpreter = {
    instructions = [ 
    Print lorem;
    Newline;
    Print prompt;
    Prompt;
    Print lorem;
    Print lorem;
    Newline;
    Print prompt;
    Prompt;
    Newline;
    Print prompt;
    Prompt;
    Newline;
    Print prompt;
    Prompt; 
    Quit
        ];
    state = Running; 
    transcript = []; 
    lines_added_since_last_input = 0;
    pending_lines = []
    };;
    
    
let draw_transcript transcript = 
    clear_graph();
    display_lines transcript 100 100 text_max_height 10;;
        
let wait_for_key interpreter = 
    draw_transcript ("--- MORE ---" :: interpreter.transcript) ;
    let status = wait_next_event events in
    let key = string_of_char status.key in
    step_with_input interpreter key;;
    
let wait_for_string interpreter max =
    let rec input_loop buffer =
        let transcript = interpreter.transcript in
        let (to_display, _) = wrap_lines (add_to_lines transcript buffer) text_max_width in
        draw_transcript to_display;
        
        let status = wait_next_event events in
        let key = string_of_char status.key in
        let length = String.length buffer in
        if key = "\r" then buffer
        else if key = "\b" then (
            if length = 0 then input_loop buffer 
            else input_loop (String.sub buffer 0 (length - 1)))
        else if length > max then input_loop buffer
        else input_loop (buffer ^ key) in
    let result = input_loop "" in
    step_with_input interpreter result;;

let rec main_loop interpreter =
    let new_interpreter = step interpreter in
    draw_transcript new_interpreter.transcript;
    match new_interpreter.state with
    | Running -> main_loop new_interpreter
    | Halted -> ()
    | Printing_is_waiting_for_key -> main_loop (wait_for_key new_interpreter)
    | Prompting_is_waiting_for_string -> main_loop (wait_for_string new_interpreter 80);;

main_loop main_interpreter;;
