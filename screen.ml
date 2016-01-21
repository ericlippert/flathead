(* TODO: Move these into a utility function module. *)

let rec times f n item =
  if n = 0 then item else times f (n - 1) (f item)

let spaces n =
  String.make n ' '

let rec reverse_index_from text target index =
  if index < 0 then None
  else if text.[index] = target then Some index
  else reverse_index_from text target (index - 1)

let left_string text length =
  String.sub text 0 length

let right_string text index =
  String.sub text index ((String.length text) - index)

let break_string text target =
  let index = String.index text target in
  let left = left_string text index in
  let right = right_string text (index + 1) in
  (left, right)

let replace_at original_text index new_text =
  let len = String.length new_text in
  let left = left_string original_text index in
  let right = right_string original_text (index + len) in
  left ^ new_text ^ right


(* A window is a rectangular block of text with a cursor. Text is
written at the cursor; text which hits the edge of the window is wrapped
either on word or character boundaries. Text written on the bottom line
may cause the window to scroll. Windows keep track of pending text and
will add it to the bottom line when scrolled. A window also knows how
many times it has been scrolled, and lets the host know when it should
be displaying "MORE". *)

type scroll_enabled =
  | Scroll_enabled
  | Scroll_disabled

type wrap_enabled =
  | Word_wrap_enabled
  | Word_wrap_disabled

type more_enabled =
  | More_enabled
  | More_disabled

type scroll_pending =
  | Scroll_pending of string
  | Nothing_pending

(* TODO: This can be its own module *)

(* Cursor position is one-based; (1, 1) is the top left,
(width, height) is the bottom right. *)

type window =
{
  cursor : int * int;
  can_wrap : wrap_enabled;
  can_scroll : scroll_enabled;
  can_more : more_enabled;
  pending : scroll_pending;
  lines : string Deque.t;
  height : int;
  width : int;
  scroll_count : int;
  needs_more : bool
}

let make_window height width x y can_wrap can_scroll can_more =
  let blank_line = spaces width in
  let add d =
    Deque.enqueue_back d blank_line in
  {
    cursor = (x, y);
    can_wrap;
    can_scroll;
    can_more;
    pending = Nothing_pending;
    lines = times add height Deque.empty;
    height;
    width;
    scroll_count = 0;
    needs_more = false
  }

let erase window =
  let blank_line = spaces window.width in
  let add d =
    Deque.enqueue_back d blank_line in
  { window with lines = times add window.height Deque.empty }

let set_window_cursor window x y =
  (* TODO: Check ranges *)
  { window with cursor = (x, y) }

let carriage_return window =
  (* We are logically executing a carriage return. There are several
    cases to consider. *)

  (* Easy case: If we are in the middle of the window, just bump it down
    one line and go all the way to the left. *)
  let (x, y) = window.cursor in
  if y < window.height then
    set_window_cursor window 1 (y + 1)

  (* We are at the bottom of the window. Second easy case: if the window cannot
    scroll then it simply stays put. *)
  else if window.can_scroll = Scroll_disabled then
    window

  (* We are at the bottom of the window and we can scroll. There are two
  remaining cases: if there is no pending input yet then make a blank
  pending input; this is the "what will be printed on the next line when
  we scroll" case. *)

  (* Otherwise, we already have a bunch of pending text to process. Simply
  add a carriage return to that and the print logic will deal with it later. *)
  else
    match window.pending with
    | Nothing_pending ->
      { window with pending = Scroll_pending "" }
    | Scroll_pending text ->
      { window with pending = Scroll_pending (text ^ "\n") }

let get_line window y =
  Deque.peek_front_at window.lines (window.height - y)

let set_line window line y =
  let lines = Deque.set_front_at window.lines line (window.height - y) in
  { window with lines }

let erase_line_window window =
  let (x, y) = window.cursor in
  let old_line = get_line window y in
  let left = left_string old_line (x - 1) in
  let right = spaces (window.width - x + 1) in
  set_line window (left ^ right) y

let rec print_window window text =
  let len = String.length text in
  if len = 0 then
    (* Base case: string we're adding is empty -> no change *)
    window
  else match window.pending with
  | Scroll_pending p ->
    (* Base case: we are already buffering output; just add to
       the buffer. *)
    { window with pending = Scroll_pending (p ^ text) }

  | _ ->

    let (x, y) = window.cursor in
    let left_in_line = window.width - x + 1 in
    if String.contains text '\n' then

      (* Recursive case: If the string contains newlines, break it
         at the newline and print both resulting strings *)
      let (left, right) = break_string text '\n' in
      print_window (carriage_return (print_window window left)) right
    else if len < left_in_line then
      (* Base case: if we are writing text that entirely fits on the
         current line then replace the portion of the current line
         at the current cursor position. *)

      let line = get_line window y in
      let new_line = replace_at line (x - 1) text in
      set_window_cursor (set_line window new_line y) (x + len) y
    else
      (* Recursive case: the text does not fit on the current line. *)
      let line = get_line window y in
      (* Construct the line that is too long for the screen *)
      let over_length_line = (left_string line (x - 1)) ^ text in
      (* Figure out where we can break this line. If wrapping, find a space
         that would be on the screen. If there is none, or we're not wrapping
         then just break at the edge of the screen. *)
      let break_index =
        if window.can_wrap = Word_wrap_enabled then
          let space_location =
            reverse_index_from over_length_line ' ' (window.width - 1) in
          match space_location with
            | None -> (window.width - 1)
            | Some location -> location
        else
          (window.width - 1) in
      (* Rewrite the current line *)
      let left = left_string over_length_line (break_index + 1) in
      let blank_line = spaces (window.width - break_index - 1)  in
      let new_line = left ^ blank_line in
      let right = right_string over_length_line (break_index + 1) in
      (* and then the unwritten remainder is dealt with recursively. *)
      print_window (carriage_return (set_line window new_line y)) right

let scroll_window window =
  (* To scroll a window we lose the top line and add
  a new blank line. The cursor is positioned at the
  bottom left corner. If there is buffered text pending
  then it is printed onto the new blank line. *)
  if window.can_scroll = Scroll_disabled then
    window
  else
    let blank_line = spaces window.width in
    let scrolled_window = { window with
      lines = Deque.enqueue_front (Deque.dequeue_back window.lines) blank_line;
      cursor = (1, window.height);
      pending = Nothing_pending;
      scroll_count = window.scroll_count + 1 ;
    } in
    let printed_window =
      match window.pending with
      | Nothing_pending -> scrolled_window
      | Scroll_pending text -> print_window scrolled_window text in
    let more_window =
      match printed_window.can_more with
      | More_disabled -> printed_window
      | More_enabled ->
        { printed_window with
          needs_more =
            printed_window.pending != Nothing_pending &&
            printed_window.scroll_count >= printed_window.height - 3 } in
    more_window

(* Sometimes it is convenient to simply say "scroll the whole thing"
and don't prompt for MORE. *)
let rec fully_scroll_window window =
  if window.pending = Nothing_pending then
    { window with scroll_count = 0; needs_more = false }
  else
    fully_scroll_window (scroll_window window)

let more_window window =
  if window.can_more = More_disabled then
    window
  else
    let blank_line = spaces (window.width - 6) in
    set_line window ("[MORE]" ^ blank_line) window.height

let clear_more_window window =
  { window with scroll_count = 0; needs_more = false }

type window_selection =
  | Upper_window
  | Lower_window

type t =
{
  status : string option;
  upper_window : window;
  lower_window : window;
  height : int;
  width : int;
  selected_window : window_selection;
}

let make height width =
  {
    status = None;
    upper_window = make_window 0 width 1 1 Word_wrap_disabled Scroll_disabled More_disabled;
    lower_window = make_window height width 1 height Word_wrap_enabled Scroll_enabled More_enabled;
    height;
    width;
    selected_window = Lower_window;
  }

let screen_lines screen =
  Deque.merge screen.upper_window.lines screen.lower_window.lines

let erase_upper screen =
  { screen with upper_window = erase screen.upper_window }

let erase_lower screen =
  { screen with lower_window = erase screen.lower_window }

let erase_all screen =
  erase_upper (erase_lower screen)

let upper_cursor screen =
  screen.upper_window.cursor

let lower_cursor screen =
  let (x, y) = screen.lower_window.cursor in
  (x, y + screen.upper_window.height)

let set_upper_cursor screen x y =
  { screen with upper_window = set_window_cursor screen.upper_window x y }

let set_lower_cursor screen x y =
  { screen with lower_window =
    set_window_cursor screen.lower_window x (y - screen.upper_window.height) }

let set_cursor screen x y =
  match screen.selected_window with
  | Lower_window -> set_lower_cursor screen x y
  | Upper_window -> set_upper_cursor screen x y

let split_window screen new_upper_height =
  (* Splitting does not change the contents of the screen, but it can
  change what window is selected and where the cursor is. *)

  (* First figure out what lines go in which window. *)
  let (new_upper, new_lower) = Deque.split (screen_lines screen) new_upper_height in

  (* If the lower window is selected when the screen is split then the
    upper window cursor is reset to 1, 1. *)

  (* Spec 8.7.2.1.1
  It is unclear exactly what split_window should do if the upper window is
  currently selected. The author suggests that it should work as usual,
  leaving the cursor where it is if the cursor is still inside the new upper
  window, and otherwise moving the cursor back to the top left. *)

  let (upper_x, upper_y) =
    match (screen.selected_window, new_upper_height) with
    | (Lower_window, _)
    | (Upper_window, 0) -> (1, 1)
    | _ ->
      let (current_upper_x, current_upper_y) = upper_cursor screen in
      if current_upper_y <= new_upper_height then
        (current_upper_x, current_upper_y)
      else
        (1, 1) in

  (* Spec 8.7.2.2
  If a split takes place which would cause the upper window to swallow the
  lower window's cursor position, the interpreter should move the lower
  window's cursor down to the line just below the upper window's new size. *)

  let (current_lower_x, current_lower_y) = lower_cursor screen in
  let (lower_x, lower_y) =
    if new_upper_height >= current_lower_y then
      (current_lower_x, new_upper_height + 1)
    else
      (current_lower_x, current_lower_y) in

  let selected_window =
    match (screen.selected_window, new_upper_height) with
    | (Upper_window, 0) -> Lower_window
    | (Upper_window, _) -> Upper_window
    | _ -> Lower_window in

  (* Put it all together *)

  let upper_window = { screen.upper_window with
    lines = new_upper;
    height = new_upper_height;
    cursor = (upper_x, upper_y) } in
  let lower_window = { screen.lower_window with
    lines = new_lower;
    height = screen.height - new_upper_height;
    cursor = (lower_x, lower_y - new_upper_height)} in
  { screen with upper_window; lower_window; selected_window }

let set_window screen w =
  (* SPEC 8.7.2 Whenever the upper window is selected, its
    cursor position is reset to the top left. *)
  let s = { screen with selected_window = w } in
  if w = Upper_window then set_upper_cursor s 1 1
  else s

let print screen text =
  match screen.selected_window with
  | Lower_window ->
    { screen with lower_window = print_window screen.lower_window text }
  | Upper_window ->
    { screen with upper_window = print_window screen.upper_window text }

let scroll screen =
  { screen with lower_window = scroll_window screen.lower_window }

let fully_scroll screen =
  { screen with lower_window = fully_scroll_window screen.lower_window }

let needs_scroll screen =
  screen.lower_window.pending != Nothing_pending

let needs_more screen =
  screen.lower_window.needs_more

let more screen =
  { screen with lower_window = more_window screen.lower_window }

let clear_more screen =
  { screen with lower_window = clear_more_window screen.lower_window }

let erase_line screen =
match screen.selected_window with
| Lower_window ->
  { screen with lower_window = erase_line_window screen.lower_window }
| Upper_window ->
  { screen with upper_window = erase_line_window screen.upper_window }
