module IntMap = Map.Make(struct type t = int let compare = compare end)
type bit_number = Bit_number of int
type bit_size = Bit_size of int
type byte_address = Byte_address of int
