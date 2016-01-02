open Graphics;;

open_graph "";;

let events = [Key_pressed];;

let string_of_char x = String.make 1 x;;

(* Takes a list of strings, the (x, y) coordinates of the bottom left
   corner of a window, the height of the window, and the number of pixels
   per line. Pops the list until the window is full or the list is empty. *)
   
let display_lines lines x y window_height line_height = 
    let rec aux lines n = 
        if n * line_height > window_height then ()
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
the tail of the list is already word-wrapped. *)
    
let rec wrap_lines lines wrap_size =
    match lines with
    | [] -> []
    | h :: t -> 
        let len = String.length h in
        if len > wrap_size then
            let space_location = reverse_index_from h ' ' wrap_size in
            let break_point = match space_location with
                | None -> wrap_size
                | Some location -> location in
            wrap_lines ((String.sub h (break_point + 1) (len - break_point - 1)) :: (String.sub h 0 break_point) :: t) wrap_size
        else
            lines;;
            
let add_to_lines lines str =
    match lines with 
    | [] -> [str]
    | h :: t -> (h ^ str) :: t ;;
          
let get_input draw max =
    let rec input_loop buffer =
        draw buffer;
        let status = wait_next_event events in
        let key = string_of_char status.key in
        let length = String.length buffer in
        if key = "x" then failwith "quitting"
        else if key = "\r" then buffer
        else if key = "\b" then (
            if length = 0 then input_loop buffer 
            else input_loop (String.sub buffer 0 (length - 1)))
        else if length > max then input_loop buffer
        else input_loop (buffer ^ key) in
    input_loop "";;

let rec main_loop transcript =
    let draw user_input =
        clear_graph ();
        let to_display = add_to_lines transcript user_input in
        let wrapped = wrap_lines to_display 40 in
        display_lines wrapped 100 100 200 10 in
    let input = get_input draw 80 in
    let result = "blah blah blah" in
    let added_input = wrap_lines (add_to_lines transcript input) 40 in
    let added_result = wrap_lines (result :: added_input) 40 in
    let added_prompt = " >" :: added_result in
    main_loop added_prompt;;    
        
main_loop [" >"];;
