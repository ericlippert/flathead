open Type;;

Graphics.open_graph "";;
Graphics.auto_synchronize false;;
Graphics.set_font "Lucida Console";;

let text_size text =
  let (w, h) = Graphics.text_size text in
  ((Pixel_width w), (Pixel_height h))

let (text_width, text_height) = text_size "X";;

let fill_rect color (Pixel_x x) (Pixel_y y) (Pixel_width w) (Pixel_height h) =
  Graphics.set_color color;
  Graphics.fill_rect x y w h

let draw_string_at text color (Pixel_x x) (Pixel_y y) =
  Graphics.set_color color;
  Graphics.moveto x y;
  Graphics.draw_string text
