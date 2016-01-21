
(* Cursor position is one-based; (1, 1) is the top left, (width, height) is the bottom right. *)

type window =
  | Upper_window
  | Lower_window

type t =
{
  status : string option;
  lines : string Deque.t;
  height : int;
  width : int;
  cursor : int * int;
  needs_scroll : bool;
  needs_more : bool;
  word_wrap : bool;
  pending : string;
  scroll_count : int;
  upper_window : int;
  selected_window : window;
  upper_cursor : int * int
}

let rec times f n item =
  if n = 0 then item else times f (n - 1) (f item)

let spaces n =
  String.make n ' '

let get_line screen y =
  Deque.peek_front_at screen.lines (screen.height - y)

let set_line screen line y =
  let lines = Deque.set_front_at screen.lines line (screen.height - y) in
  { screen with lines = lines }

let erase_upper screen =
  let blank_line = spaces screen.width in
  let removed = times Deque.dequeue_back screen.upper_window screen.lines in
  let add d =
    Deque.enqueue_back d blank_line in
  { screen with lines = times add screen.upper_window removed }

let erase_lower screen =
  let lower_size = screen.height - screen.upper_window in
  let blank_line = spaces screen.width in
  let removed = times Deque.dequeue_front lower_size screen.lines in
  let add d =
    Deque.enqueue_front d blank_line in
  { screen with lines = times add lower_size removed }

let erase_all screen =
  let blank_line = spaces screen.width in
  let add d =
    Deque.enqueue_front d blank_line in
  { screen with lines = times add screen.height Deque.empty }

let make height width =
  let blank_line = spaces width in
  let add d =
    Deque.enqueue_front d blank_line in
  {
    status = None;
    lines = times add height Deque.empty;
    height = height;
    width = width;
    cursor = (1, height);
    needs_scroll = false;
    needs_more = false;
    word_wrap = true;
    pending = "";
    scroll_count = 0;
    upper_window = 0;
    selected_window = Lower_window;
    upper_cursor = (1, 1)
  }

let set_upper_cursor screen x y =
  { screen with upper_cursor = (x, y) }

let set_lower_cursor screen x y =
  { screen with cursor = (x, y) }

let set_cursor screen x y =
  (* TODO: Verify that coordinates are within bounds.*)
  match screen.selected_window with
  | Lower_window -> set_lower_cursor screen x y
  | Upper_window -> set_upper_cursor screen x y

let active_cursor screen =
  match screen.selected_window with
  | Lower_window -> screen.cursor
  | Upper_window -> screen.upper_cursor

let carriage_return screen =
  (* TODO: This scrolling logic is not right *)
  let (_, y) = active_cursor screen in
  if screen.needs_scroll then
    { screen with pending = screen.pending ^ "\n" }
  else if y = screen.height then
    { screen with needs_scroll = true }
  else
    { screen with cursor = (1, y + 1) }

let rec print screen text =
  let rec reverse_index_from text target index =
    if index < 0 then None
    else if text.[index] = target then Some index
    else reverse_index_from text target (index - 1) in
  let left_string text length =
    String.sub text 0 length in

  let right_string text index =
    String.sub text index ((String.length text) - index) in

  let break_string text target =
    let index = String.index text target in
    let left = left_string text index in
    let right = right_string text (index + 1) in
    (left, right) in

  let len = String.length text in
  if len = 0 then
    (* Base case: string we're adding is empty -> no change *)
    screen
  else if screen.needs_scroll then
    (* Base case: we are already buffering output; just add to
       the buffer. *)
    { screen with pending = screen.pending ^ text }
  else
    let (x, y) = active_cursor screen in
    let left_in_line = screen.width - x + 1 in
    if String.contains text '\n' then
      (* Recursive case: If the string contains newlines, break it
         at the newline and print both resulting strings *)
      let (left, right) = break_string text '\n' in
      print (carriage_return (print screen left)) right
    else if len < left_in_line then
      (* Base case: if we are writing text that entirely fits on the
         current line then replace the portion of the current line
         at the current cursor position. *)
      let line = get_line screen y in
      let left = left_string line (x - 1) in
      let right = right_string line (x - 1 + len) in
      let new_line = left ^ text ^ right in
      let s = set_line screen new_line y in
      set_cursor s (x + len) y
    else
      (* Recursive case: the text does not fit on the current line. *)
      let line = get_line screen y in
      (* Construct the line that is too long for the screen *)
      let over_length_line = (left_string line (x - 1)) ^ text in
      (* Figure out where we can break this line. If wrapping, find a space
         that would be on the screen. If there is none, or we're not wrapping
         then just break at the edge of the screen. *)
      let break_index =
        if screen.word_wrap then
          let space_location = reverse_index_from over_length_line ' ' (screen.width - 1) in
          match space_location with
            | None -> (screen.width - 1)
            | Some location -> location
        else
          (screen.width - 1) in
      (* Rewrite the current line *)
      let left = left_string over_length_line (break_index + 1) in
      let blank_line = spaces (screen.width - break_index - 1)  in
      let new_line = left ^ blank_line in
      let right = right_string over_length_line (break_index + 1) in
      (* and then the unwritten remainder is dealt with recursively. *)
      print (carriage_return (set_line screen new_line y)) right

let scroll screen =
  (* To scroll the screen we lose the top line and add
  a new blank line. The cursor is positioned at the
  bottom left corner. If there is buffered text pending
  then it is printed onto the new blank line. *)
  let blank_line = spaces screen.width in
  let new_screen = { screen with
    lines = Deque.enqueue_front (Deque.dequeue_back screen.lines) blank_line;
    cursor = (1, screen.height);
    needs_scroll = false;
    pending = "";
    scroll_count = screen.scroll_count + 1 ;
  } in
  let printed_screen = print new_screen screen.pending in
  { printed_screen with
    needs_more = printed_screen.needs_scroll && printed_screen.scroll_count >= printed_screen.height - 3 }

let rec fully_scroll screen =
  if screen.needs_scroll then fully_scroll (scroll screen)
  else { screen with scroll_count = 0 }

let more screen =
  let blank_line = spaces (screen.width - 6) in
  set_line screen ("[MORE]" ^ blank_line) screen.height

let set_window screen w =
  (* SPEC 8.7.2 Whenever the upper window is selected, its
    cursor position is reset to the top left. *)
  let s = { screen with selected_window = w } in
  if w = Upper_window then set_upper_cursor s 1 1
  else s

let split_window screen lines =
  let s = match (screen.selected_window, lines) with
  | (Lower_window, _) ->
    { screen with
      upper_window = lines;
      upper_cursor = (1, 1) }
  | (Upper_window, 0) ->
    { screen with
      selected_window = Lower_window;
      upper_window = 0;
      upper_cursor = (1, 1) }
  | (Upper_window, _) ->
    (* Spec 8.7.2.1.1
    It is unclear exactly what split_window should do if the upper window is
    currently selected. The author suggests that it should work as usual,
    leaving the cursor where it is if the cursor is still inside the new upper
    window, and otherwise moving the cursor back to the top left. *)
    let (_, upper_y) = screen.upper_cursor in
    if upper_y <= lines then
      { screen with upper_window = lines }
    else
      { screen with
        upper_window = lines;
        upper_cursor = (1, 1) } in
  (* Spec 8.7.2.2
  If a split takes place which would cause the upper window to swallow the
  lower window's cursor position, the interpreter should move the lower
  window's cursor down to the line just below the upper window's new size. *)
  let (lower_x, lower_y) = s.cursor in
  if lines >= lower_y && lines < s.height then
    { s with cursor = (lower_x, lines + 1) }
  else
    s
