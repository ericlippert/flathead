
let take items n = 
    let rec aux items n acc = 
    match (items, n) with
    | (_, 0) -> acc
    | ([], _) -> acc
    | ((h :: t), _) -> aux t (n - 1) (h :: acc) in
    List.rev (aux items n []);;
    
let rec drop items n = 
    match(items, n) with
    | (_, 0) -> items
    | ([], _) -> []
    | (h :: t, _) -> drop t (n - 1);;

module Deque = struct

    (* Simplified version of Chris Okasaki's deque. *)

    let c = 3;;

    type 'a t = 
    { 
        front : 'a list;
        front_length : int;
        back : 'a list;
        back_length : int
    };;
    
    (* Invariants: front_length and back_length are the lengths of the lists *)
    (* Invariants: front_length <= c * back_length + 1 *)
    (* Invariants: back_length <= c * front_length + 1 *)
    
    exception Empty;;
   
    let empty = { front = []; front_length = 0; back = []; back_length = 0 };;
    
    let is_empty deque = deque.front_length + deque.back_length = 0;;
    
    let balance deque = 
        if deque.front_length > c * deque.back_length + 1 then
            let new_front_length = (deque.front_length + deque.back_length) / 2 in
            let new_back_length = deque.front_length + deque.back_length - new_front_length in
            { 
                front = take deque.front new_front_length; 
                front_length = new_front_length; 
                back = deque.back @ (List.rev (drop deque.front new_front_length)); 
                back_length = new_back_length
            }
        else if deque.back_length > c * deque.front_length + 1 then
            let new_front_length = (deque.front_length + deque.back_length) / 2 in
            let new_back_length = deque.front_length + deque.back_length - new_front_length in
            { 
                front = deque.front @ List.rev(drop deque.back new_back_length);
                front_length = new_front_length; 
                back = take deque.back new_back_length;
                back_length = new_back_length
            }
        else deque;;
        
    let enqueue_front deque item = 
        balance { deque with front = item :: deque.front; front_length = deque.front_length + 1};;
        
    let enqueue_back deque item = 
        balance { deque with back = item :: deque.back; back_length = deque.back_length + 1};;
        
    let peek_front deque = 
        match deque with
        | { front = []; back = [] } -> raise Empty 
        | { front = h :: _} -> h
        | { back = [h] } -> h
        | _ -> failwith "peek_front: Front is empty, back has more than one item";;
        
    let peek_back deque = 
        match deque with
        | { front = []; back = [] } -> raise Empty 
        | { back = h :: _} -> h
        | { front = [h] } -> h
        | _ -> failwith "peek_back: Back is empty, front has more than one item";;
        
    let dequeue_front deque = 
        match deque with
        | { front = []; back = [] } -> raise Empty 
        | { front = [_]; back = [] } -> empty
        | { front = []; back = [_] } -> empty
        | { front = _ :: t } -> balance { deque with front = t; front_length = deque.front_length - 1 }
        | _ -> failwith "dequeue_front: Front is empty, back has more than one item";;
           
    let dequeue_back deque = 
        match deque with
        | { front = []; back = [] } -> raise Empty 
        | { front = [_]; back = [] } -> empty
        | { front = []; back = [_] } -> empty
        | { back = _ :: t } -> balance { deque with back = t; back_length = deque.back_length - 1 }
        | _ -> failwith "dequeue_back: Back is empty, front has more than one item";;
           
    let rec peek_front_at deque n =
        if n = 0 then peek_front deque
        else peek_front_at (dequeue_front deque) (n - 1);;
        
    let rec peek_back_at deque n =
        if n = 0 then peek_back deque
        else peek_back_at (dequeue_back deque) (n - 1);;
        
    let rec set_front_at deque item n =
        if n = 0 then enqueue_front (dequeue_front deque) item
        else enqueue_front (set_front_at (dequeue_front deque) item (n - 1)) (peek_front deque);;
        
    let rec set_back_at deque item n = 
        if n = 0 then enqueue_back (dequeue_back deque) item
        else enqueue_back (set_back_at (dequeue_back deque) item (n - 1)) (peek_back deque);;
        
end

open Graphics;;

open_graph "";;

set_font "Lucida Console";;

let events = [Key_pressed];;

let string_of_char x = String.make 1 x;;

let text_max_height = 10;;

let text_max_width = 40;;

(* Takes a string, a character and an index; finds
the highest index that matches the character at or before
the given index. If there is no match then None is returned. *)

let rec reverse_index_from text target index =
    if index < 0 then None
    else if text.[index] = target then Some index
    else reverse_index_from text target (index - 1);;

let enqueue_duplicate item times =
    let rec aux n deque =
        if n = 0 then deque 
        else aux (n - 1) (Deque.enqueue_front deque item) in
    aux times Deque.empty;

module Screen = struct

    open Deque;;

    (* Cursor position is one-based; (1, 1) is the top left, (width, height) is the bottom right. *)

    type t =
    {
        lines : string Deque.t;
        height : int;
        width : int;
        cursor : int * int;
        needs_scroll : bool;
        word_wrap : bool;
        pending : string;
        scroll_count : int
    };;
    
    let make height width = 
    {
        lines = enqueue_duplicate (String.make width ' ') height;
        height = height;
        width = width;
        cursor = (1, height);
        needs_scroll = false;
        word_wrap = true;
        pending = "";
        scroll_count = 0
    };;
    
    let carriage_return screen = 
        let (_, y) = screen.cursor in
        if screen.needs_scroll then 
            { screen with pending = screen.pending ^ "\r" }
        else if y = screen.height then
            { screen with needs_scroll = true }
        else 
            { screen with cursor = (1, y + 1) };;

    let rec print screen text = 
        let len = String.length text in
        if len = 0 then 
            screen 
        else if screen.needs_scroll then 
            { screen with pending = screen.pending ^ text }
        else 
            let (x, y) = screen.cursor in
            let left_in_line = screen.width - x + 1 in
            if String.contains text '\r' then
                let b = String.index text '\r' in
                let f = String.sub text 0 b in
                let r = String.sub text (b + 1) (len - b - 1) in
                let s1 = print screen f in
                let s2 = carriage_return s1 in
                print s2 r
            else if len >= left_in_line then
                let line = peek_front_at screen.lines (screen.height - y) in
                let over_length_line = (String.sub line 0 (x - 1)) ^ text in
                let b = if screen.word_wrap then  
                    let space_location = reverse_index_from over_length_line ' ' (screen.width - 1) in
                    match space_location with None -> (screen.width - 1) | Some location -> location 
                else 
                    (screen.width - 1) in
                let new_line = (String.sub over_length_line 0 (b + 1)) ^ (String.make (screen.width - b - 1) ' ')  in
                let r = String.sub over_length_line (b + 1) ((String.length over_length_line) - b - 1) in
                let s1 = { screen with lines = set_front_at screen.lines new_line (screen.height - y) } in
                let s2 = carriage_return s1 in
                print s2 r
            else
                let line = peek_front_at screen.lines (screen.height - y) in
                let new_line = (String.sub line 0 (x - 1)) ^ text ^ (String.sub line (x + len - 1) (screen.width - x - len + 1)) in
                { screen with 
                    lines = set_front_at screen.lines new_line (screen.height - y);
                    cursor = (x + len, y)
                };;
            
    let scroll screen = 
        let new_screen = { screen with 
            lines = enqueue_front (dequeue_back screen.lines) (String.make screen.width ' ');
            cursor = (1, screen.height);
            needs_scroll = false;
            pending = "";
            scroll_count = screen.scroll_count + 1 } in
        print new_screen screen.pending;;

    let rec fully_scroll screen =
        if screen.needs_scroll then fully_scroll (scroll screen) 
        else { screen with scroll_count = 0};;
        
    let set_cursor screen x y = 
        { (fully_scroll screen) with cursor = (x, y) };;
        
    let more screen = 
        { screen with lines = set_front_at screen.lines ("[MORE]" ^ (String.make (screen.width - 6) ' ')) 0 };;

end

open Screen ;;    
    
type instruction = 
    | Print of string
    | Newline
    | Prompt 
    | Quit;;
    
type interpreter_state =
    | Running
    | Halted
    | Prompting_is_waiting_for_string ;;
    
type interpreter =
{
    instructions : instruction list;
    state : interpreter_state;
    screen : Screen.t
};;

let next_instruction interpreter =
    { interpreter with instructions = List.tl interpreter.instructions; state = Running };;

let do_print interpreter text = 
    { interpreter with screen = Screen.print interpreter.screen text };;
    
let do_newline interpreter =
    { interpreter with screen = Screen.carriage_return interpreter.screen };;
    
let complete_prompting interpreter s =
    next_instruction interpreter;;
   
let step interpreter =
    match (interpreter.state, interpreter.instructions) with
    | (_, [])  
    | (Halted, _) -> failwith "halted"
    | (Prompting_is_waiting_for_string, _) -> failwith "waiting for string"
    | (Running, (Print s) :: _) -> next_instruction (do_print interpreter s)
    | (Running, Newline :: _) -> next_instruction (do_newline interpreter  )
    | (Running, Prompt :: _) -> { interpreter with state = Prompting_is_waiting_for_string }
    | (Running, Quit :: _ ) -> { interpreter with state = Halted };;
    
let step_with_input interpreter input =
    match (interpreter.state, interpreter.instructions) with
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
    screen = Screen.make 20 20
    
    };;
    
let draw_screen screen = 
    clear_graph();
    let rec aux deque n = 
        if Deque.is_empty deque then ()
        else (
            moveto 100 (100 + 12 * n);
            draw_string (Deque.peek_front deque);
            aux (Deque.dequeue_front deque) (n + 1) ) in
    aux screen.lines 0;;

let rec draw_screen_with_scrolling screen =
    if screen.needs_scroll then (
        if screen.scroll_count >= (screen.height - 2) then (
            let _ = draw_screen (more screen) in
            let _ = wait_next_event events in
            draw_screen_with_scrolling (scroll { screen with scroll_count = 0 }) )
        else
        (
            let scrolled = scroll screen in
            draw_screen scrolled;
            draw_screen_with_scrolling scrolled
        )
    )
    else ( draw_screen screen; screen );;

let wait_for_string screen max =
    let scrolled = draw_screen_with_scrolling screen in
    let rec input_loop buffer =
        let screen_with_buffer = fully_scroll (print scrolled buffer) in
        draw_screen screen_with_buffer;
        let status = wait_next_event events in
        let key = string_of_char status.key in
        let length = String.length buffer in
        if key = "\r" then (buffer, (carriage_return screen_with_buffer))
        else if key = "\b" then (
            if length = 0 then input_loop buffer 
            else input_loop (String.sub buffer 0 (length - 1)))
        else if length > max then input_loop buffer
        else input_loop (buffer ^ key) in
    input_loop "";;

let rec main_loop interpreter =
    let stepped_interpreter = step interpreter in
    let printed_screen = draw_screen_with_scrolling stepped_interpreter.screen in
    match stepped_interpreter.state with
    | Running -> main_loop { stepped_interpreter with screen = printed_screen }
    | Halted -> ()
    | Prompting_is_waiting_for_string -> 
        let (input, input_screen) = wait_for_string printed_screen 80 in
        main_loop (step_with_input { stepped_interpreter with screen = input_screen } input);;

main_loop main_interpreter;; 

