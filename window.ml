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

let make height width x y can_wrap can_scroll can_more =
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

let set_cursor window x y =
  (* TODO: Check ranges *)
  { window with cursor = (x, y) }

let carriage_return window =
  (* We are logically executing a carriage return. There are several
    cases to consider. *)

  (* Easy case: If we are in the middle of the window, just bump it down
    one line and go all the way to the left. *)
  let (x, y) = window.cursor in
  if y < window.height then
    set_cursor window 1 (y + 1)

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

let erase_line window =
  let (x, y) = window.cursor in
  let old_line = get_line window y in
  let left = left_string old_line (x - 1) in
  let right = spaces (window.width - x + 1) in
  set_line window (left ^ right) y

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

    let (x, y) = window.cursor in
    let left_in_line = window.width - x + 1 in
    if String.contains text '\n' then

      (* Recursive case: If the string contains newlines, break it
         at the newline and print both resulting strings *)
      let (left, right) = break_string text '\n' in
      print (carriage_return (print window left)) right
    else if len < left_in_line then
      (* Base case: if we are writing text that entirely fits on the
         current line then replace the portion of the current line
         at the current cursor position. *)

      let line = get_line window y in
      let new_line = replace_at line (x - 1) text in
      set_cursor (set_line window new_line y) (x + len) y
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
      print (carriage_return (set_line window new_line y)) right

let scroll window =
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
      | Scroll_pending text -> print scrolled_window text in
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
let rec fully_scroll window =
  if window.pending = Nothing_pending then
    { window with scroll_count = 0; needs_more = false }
  else
    fully_scroll (scroll window)

let more window =
  if window.can_more = More_disabled then
    window
  else
    let blank_line = spaces (window.width - 6) in
    set_line window ("[MORE]" ^ blank_line) window.height

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
  { window with lines = new_lines; height = Deque.length new_lines }

let has_pending window =
  window.pending != Nothing_pending

let needs_more window =
  window.needs_more

let set_can_wrap window can_wrap =
  { window with can_wrap }
