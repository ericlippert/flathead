open Type;;

Graphics.open_graph "";;
Graphics.auto_synchronize false;;
Graphics.set_font "Lucida Console";;

let text_size text =
  let (w, h) = Graphics.text_size text in
  ((Pixel_width w), (Pixel_height h))

let (text_width, text_height) = text_size "0";;

let fill_rect color (Pixel_x x) (Pixel_y y) (Pixel_width w) (Pixel_height h) =
  Graphics.set_color color;
  Graphics.fill_rect x y w h

let draw_string_at text color (Pixel_x x) (Pixel_y y) =
  Graphics.set_color color;
  Graphics.moveto x y;
  Graphics.draw_string text

let add_pixels_h (Pixel_height h1) (Pixel_height h2) =
  Pixel_height (h1 + h2)

let add_pixels_w (Pixel_width w1) (Pixel_width w2) =
  Pixel_width (w1 + w2)

let add_pixels_y (Pixel_y y) (Pixel_height h) =
  Pixel_y (y + h)

let add_pixels_x (Pixel_x x) (Pixel_width w) =
  Pixel_x (x + w)

(* Given two widths, what is the margin width that centers one in the other? *)
let margin_w (Pixel_width w1) (Pixel_width w2) =
  Pixel_width ((w1 - w2) / 2)

let margin_h (Pixel_height h1) (Pixel_height h2) =
  Pixel_height ((h1 - h2) / 2)

(* Given a width and a margin, what is the total width? *)
let add_margin_w (Pixel_width w) (Pixel_width m) =
  Pixel_width (w + 2 * m)

let add_margin_h (Pixel_height h) (Pixel_height m) =
  Pixel_height (h + 2 * m)

let contains_x (Pixel_x x) (Pixel_width w) (Pixel_x t) =
    x <= t && t < (x + w)

let contains_y (Pixel_y y) (Pixel_height h) (Pixel_y t) =
    y <= t && t < (y + h)

let chars_to_pixels_h (Character_height h) =
  let (Pixel_height th) = text_height in
  Pixel_height (th * h)

let chars_to_pixels_w (Character_width w) =
  let (Pixel_width tw) = text_width in
  Pixel_width (tw * w)
