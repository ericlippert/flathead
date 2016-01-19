type t =
{
  x : int;
  y : int;
  width : int;
  height : int;
  text : string
}

let make x y margin text =
  let (text_width, text_height) = Graphics.text_size text in
  {
    x = x;
    y = y;
    width = text_width + 2 * margin;
    height = text_height + 2 * margin;
    text = text
  }

let draw_string_at text x y =
  Graphics.moveto x y;
  Graphics.draw_string text

let draw button =
  let (text_width, text_height) = Graphics.text_size button.text in
  Graphics.set_color Graphics.blue;
  Graphics.fill_rect button.x button.y button.width button.height;
  let text_x = button.x + (button.width - text_width) / 2 in
  let text_y = button.y + (button.height - text_height) / 2 in
  Graphics.set_color Graphics.white;
  draw_string_at button.text text_x text_y;
  Graphics.set_color Graphics.foreground

let was_clicked button x y =
  (button.x <= x) &&
  (x <= (button.x + button.width)) &&
  (button.y <= y) &&
  (y <= button.y + button.height)
