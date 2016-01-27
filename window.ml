open Utility
open Type

(* A window is a rectangular block of text with a cursor. Text is
written at the cursor; text which hits the edge of the window is wrapped
either on word or character boundaries. Text written on the bottom line
may cause the window to scroll. Windows keep track of pending text and
will add it to the bottom line when scrolled. A window also knows how
many times it has been scrolled, and lets the host know when it should
be displaying "MORE". *)

(* Cursor position is one-based; (1, 1) is the top left,
(width, height) is the bottom right. *)

type t =
{
  cursor : cursor;
  can_wrap : wrap_enabled;
  can_scroll : scroll_enabled;
  can_more : more_enabled;
  pending : scroll_pending;
  lines : string Deque.t;
  height : character_height;
  width : character_width;
  scroll_count : int;
  needs_more : bool
}

let make height width cursor can_wrap can_scroll can_more =
  let (Character_width w) = width in
  let (Character_height h) = height in
  let blank_line = Utility.spaces w in
  let add d =
    Deque.enqueue_back d blank_line in
  {
    cursor;
    can_wrap;
    can_scroll;
    can_more;
    pending = Nothing_pending;
    lines = times add h Deque.empty;
    height;
    width;
    scroll_count = 0;
    needs_more = false
  }

let spaces (Character_width w) =
  Utility.spaces w

let blank_line window =
  spaces window.width

let left_column = (Character_x 1)

let right_column window =
  let (Character_width w) = window.width in
  (Character_x w)

let top_row = (Character_y 1)

let bottom_row window =
  let (Character_height h) = window.height in
  (Character_y h)

let top_left = Cursor (left_column, top_row)

let bottom_left window =
  Cursor (left_column, (bottom_row window))

let set_cursor_x cursor x =
  let Cursor (_, y) = cursor in
  Cursor (x, y)

let set_cursor_y cursor y =
  let Cursor (x, _) = cursor in
  Cursor (x, y)

let add_characters_w (Character_width w1) (Character_width w2) =
  Character_width (w1 + w2)

let add_characters_h (Character_height h1) (Character_height h2) =
  Character_height (h1 + h2)

let add_characters_x (Character_x x) (Character_width w) =
  Character_x (x + w)

let add_characters_y (Character_y y) (Character_height h) =
  Character_y (y + h)

let add_characters_y_bounded (Character_y y) (Character_height b) (Character_height h) =
  let y = y + h in
  let y = max y 1 in
  let y = min y b in
  Character_y y

let add_characters_x_bounded (Character_x x) (Character_width b) (Character_width w) =
  let x = x + w in
  let x = max x 1 in
  let x = min x b in
  Character_x x

let add_characters_y_bounded (Character_y y) (Character_height b) (Character_height h) =
  let y = y + h in
  let y = max y 1 in
  let y = min y b in
  Character_y y

let move_window_cursor_x window w =
  let Cursor (x, y) = window.cursor in
  let x = add_characters_x_bounded x window.width w in
  { window with cursor = Cursor (x, y) }

let move_window_cursor_y window h =
  let Cursor (x, y) = window.cursor in
  let y = add_characters_y_bounded y window.height h in
  { window with cursor = Cursor (x, y) }

let move_cursor_down cursor h =
  let Cursor (_, y) = cursor in
  set_cursor_y cursor (add_characters_y y h)

let move_cursor_up cursor (Character_height h) =
  let h = Character_height (0 - h) in
  move_cursor_down cursor h

let return_cursor window =
  let window = move_window_cursor_y window (Character_height 1) in
  let cursor = set_cursor_x window.cursor left_column in
  { window with cursor }

let erase window =
  let (Character_height h) = window.height in
  let blank_line = blank_line window in
  let add d =
    Deque.enqueue_back d blank_line in
  { window with lines = times add h Deque.empty }

let set_cursor window cursor =
  { window with cursor }

let cursor_at_bottom window =
  let Cursor (_, y) = window.cursor in
  y = (bottom_row window)

let set_window_cursor_top_left window =
  set_cursor window top_left

let set_window_cursor_bottom_left window =
  set_cursor window (bottom_left window)

let carriage_return window =
  (* We are logically executing a carriage return. There are several
    cases to consider. *)

  (* Easy case: If we are in the middle of the window, just bump it down
    one line and go all the way to the left. *)
  if not (cursor_at_bottom window) then
    return_cursor window

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

let get_line window (Character_y y) =
  let (Character_height h) = window.height in
  Deque.peek_front_at window.lines (h - y)

let set_line window line (Character_y y) =
  let (Character_height h) = window.height in
  let lines = Deque.set_front_at window.lines line (h - y) in
  { window with lines }

let left_in_line window =
  let Cursor ((Character_x x), _) = window.cursor in
  let (Character_width w) = window.width in
  Character_width (w - x + 1)

let has_room_for window text =
  let (Character_width remaining) = left_in_line window in
  let len = String.length text in
  len < remaining

let current_line window =
  let Cursor (_, y) = window.cursor in
  get_line window y

let replace_text window text =
  let len = Character_width (String.length text) in
  let Cursor ((Character_x x), y) = window.cursor in
  let line = current_line window in
  let new_line = replace_at line (x - 1) text in
  let window = set_line window new_line y in
  move_window_cursor_x window len

let erase_line window =
  let Cursor ((Character_x x), y) = window.cursor in
  let old_line = get_line window y in
  let left = left_string old_line (x - 1) in
  let (Character_width w) = window.width in
  let right = spaces (Character_width (w - x + 1)) in
  set_line window (left ^ right) y

let find_break_index window text =
  let (Character_width w) = window.width in
  if window.can_wrap = Word_wrap_enabled then
    let space_location = reverse_index_from text ' ' (w - 1) in
    match space_location with
      | None -> (w - 1)
      | Some location -> location
  else
    (w - 1)

let line_left_of_cursor window =
  let Cursor ((Character_x x), y) = window.cursor in
  let line = get_line window y in
  left_string line (x - 1)

let rec print window text =
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
    let Cursor ((Character_x x), y) = window.cursor in
    let (Character_width w) = window.width in
    if String.contains text '\n' then
      (* Recursive case: If the string contains newlines, break it
         at the newline and print both resulting strings *)
      let (left, right) = break_string text '\n' in
      print (carriage_return (print window left)) right
    else if has_room_for window text then
      (* Base case: if we are writing text that entirely fits on the
         current line then replace the portion of the current line
         at the current cursor position. *)
      replace_text window text
    else
      (* Recursive case: the text does not fit on the current line. *)
      (* Construct the line that is too long for the screen *)
      let over_length_line = (line_left_of_cursor window) ^ text in
      (* Figure out where we can break this line. If wrapping, find a space
         that would be on the screen. If there is none, or we're not wrapping
         then just break at the edge of the screen. *)
      let break_index = find_break_index window over_length_line in
      (* We need to rewrite the entire line. If a previous write ended
      the line with "AAA BBB" and then CCC was written, with no spaces,
      we might have to move BBBCCC down to the next line. *)
      let left = left_string over_length_line (break_index + 1) in
      let blank_line = Utility.spaces (w - break_index - 1)  in
      let new_line = left ^ blank_line in
      let right = right_string over_length_line (break_index + 1) in
      (* and then the unwritten remainder is dealt with recursively. *)
      print (carriage_return (set_line window new_line y)) right

let scroll window =
  (* To scroll a window we lose the top line and add
  a new blank line. The cursor is positioned at the
  bottom left corner. If there is buffered text pending
  then it is printed onto the new blank line. *)
  if window.can_scroll = Scroll_disabled then
    window
  else
    let blank_line = blank_line window in
    let scrolled_window = { window with
      lines = Deque.enqueue_front (Deque.dequeue_back window.lines) blank_line;
      cursor = bottom_left window;
      pending = Nothing_pending;
      scroll_count = window.scroll_count + 1 ;
    } in
    let printed_window =
      match window.pending with
      | Nothing_pending -> scrolled_window
      | Scroll_pending text -> print scrolled_window text in
    let more_window =
      match printed_window.can_more with
      | More_disabled -> printed_window
      | More_enabled ->
        let (Character_height h) = printed_window.height in
        { printed_window with
          needs_more =
            printed_window.pending != Nothing_pending &&
            printed_window.scroll_count >= h - 3 } in
    more_window

(* Sometimes it is convenient to simply say "scroll the whole thing"
and don't prompt for MORE. *)
let rec fully_scroll window =
  if window.pending = Nothing_pending then
    { window with scroll_count = 0; needs_more = false }
  else
    fully_scroll (scroll window)

let more window =
  if window.can_more = More_disabled then
    window
  else
    let blank_line = blank_line window in
    let more_line = replace_at blank_line 0 "[MORE]" in
    set_line window more_line (bottom_row window)

let clear_more window =
  { window with scroll_count = 0; needs_more = false }

(* Produces a deque of the merged window lines *)
let merge w1 w2 =
  Deque.merge w1.lines w2.lines

let cursor window =
  window.cursor

let height window =
  window.height

let set_lines window new_lines =
  { window with
    lines = new_lines;
    height = Character_height (Deque.length new_lines) }

let has_pending window =
  window.pending != Nothing_pending

let needs_more window =
  window.needs_more

let set_can_wrap window can_wrap =
  { window with can_wrap }

let cursor_in_bounds window cursor =
  let (Character_height h) = window.height in
  let Cursor (_, (Character_y y)) = cursor in
  y <= h
