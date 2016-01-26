(* See wikipedia article on xorshift *)

type t =
{
  w : Int32.t;
  x : Int32.t;
  y : Int32.t;
  z : Int32.t
}

(* Produces a non-negative random integer between 1 and n,
and steps the generator. *)
let next r n =
  let xor = Int32.logxor in
  let sl = Int32.shift_left in
  let sr = Int32.shift_right_logical in
  let rem = Int32.rem in
  let to_int = Int32.to_int in
  let of_int = Int32.of_int in
  let t = xor r.x (sl r.x 11) in
  let x = r.y in
  let y = r.z in
  let z = r.w in
  let w = xor (xor (xor r.w (sr r.w 19)) t) (sr t 8) in
  let r = 1 + ((to_int (rem w (of_int n)) + n) mod n) in
  (r, { w; x; y; z })

let make_seeded seed =
  let of_int = Int32.of_int in
  { w = of_int seed; x = of_int 123; y = of_int 123; z = of_int 123 }

let make_random () =
  Random.self_init();
  let seed = Random.int 1000000 in
  make_seeded seed
