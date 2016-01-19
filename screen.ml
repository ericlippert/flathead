(* Cursor position is one-based; (1, 1) is the top left, (width, height) is the bottom right. *)
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
  scroll_count : int
}

let spaces n =
  String.make n ' '

let get_line screen y =
  Deque.peek_front_at screen.lines (screen.height - y)

let set_line screen line y =
  let lines = Deque.set_front_at screen.lines line (screen.height - y) in
  { screen with lines = lines }

let make height width =
  let blank_line = spaces width in
  let enqueue_duplicate item times =
    let rec aux n deque =
      if n = 0 then deque
      else aux (n - 1) (Deque.enqueue_front deque item) in
    aux times Deque.empty in
  {
    status = None;
    lines = enqueue_duplicate blank_line height;
    height = height;
    width = width;
    cursor = (1, height);
    needs_scroll = false;
    needs_more = false;
    word_wrap = true;
    pending = "";
    scroll_count = 0
  }

let carriage_return screen =
  let (_, y) = screen.cursor in
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
    let (x, y) = screen.cursor in
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
      { (set_line screen new_line y) with cursor = (x + len, y) }
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

let set_cursor screen x y =
  { (fully_scroll screen) with cursor = (x, y) }

let more screen =
  let blank_line = spaces (screen.width - 6) in
  set_line screen ("[MORE]" ^ blank_line) screen.height
