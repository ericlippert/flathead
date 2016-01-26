open Utility

type status_line =
  Status of string option

type window_selection =
  | Upper_window
  | Lower_window

type t =
{
  status : status_line;
  upper_window : Window.t;
  lower_window : Window.t;
  height : int;
  width : int;
  selected_window : window_selection;
}

let make height width =
  {
    status = Status None;
    upper_window = Window.make 0 width 1 1 Window.Word_wrap_disabled Window.Scroll_disabled Window.More_disabled;
    lower_window = Window.make height width 1 height Window.Word_wrap_enabled Window.Scroll_enabled Window.More_enabled;
    height;
    width;
    selected_window = Lower_window
  }

let screen_lines screen =
  Window.merge screen.upper_window screen.lower_window

let erase_upper screen =
  { screen with upper_window = Window.erase screen.upper_window }

let erase_lower screen =
  { screen with lower_window = Window.erase screen.lower_window }

let erase_all screen =
  erase_upper (erase_lower screen)

let upper_cursor screen =
  Window.cursor screen.upper_window

let lower_cursor screen =
  let (x, y) = Window.cursor screen.lower_window in
  (x, y + Window.height screen.upper_window)

let set_upper_cursor screen x y =
  { screen with upper_window = Window.set_cursor screen.upper_window x y }

let set_lower_cursor screen x y =
  { screen with lower_window =
    let h = Window.height screen.upper_window in
    Window.set_cursor screen.lower_window x (y - h) }

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

  let upper_window = Window.set_lines screen.upper_window new_upper in
  let upper_window = Window.set_cursor upper_window upper_x upper_y in
  let lower_window = Window.set_lines screen.lower_window new_lower in
  let lower_window = Window.set_cursor lower_window lower_x (lower_y - new_upper_height) in
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
    { screen with lower_window = Window.print screen.lower_window text }
  | Upper_window ->
    { screen with upper_window = Window.print screen.upper_window text }

let scroll screen =
  { screen with lower_window = Window.scroll screen.lower_window }

let fully_scroll screen =
  { screen with lower_window = Window.fully_scroll screen.lower_window }

let needs_scroll screen =
  Window.has_pending screen.lower_window

let needs_more screen =
  Window.needs_more screen.lower_window

let more screen =
  { screen with lower_window = Window.more screen.lower_window }

let clear_more screen =
  { screen with lower_window = Window.clear_more screen.lower_window }

let erase_line screen =
match screen.selected_window with
| Lower_window ->
  { screen with lower_window = Window.erase_line screen.lower_window }
| Upper_window ->
  { screen with upper_window = Window.erase_line screen.upper_window }

let get_active_cursor screen =
  match screen.selected_window with
  | Lower_window -> Window.cursor screen.lower_window
  | Upper_window -> Window.cursor screen.upper_window

let set_word_wrap screen can_wrap =
  let lower_window = Window.set_can_wrap screen.lower_window can_wrap in
  { screen with lower_window }
