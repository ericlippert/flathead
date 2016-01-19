(* The Z Machine divides memory into dynamic and static; dynamic is always
before static memory. Static memory may not change. We therefore model
memory as a dynamic block that tracks updates and a static block that
never changes at all. *)

type t =
{
  dynamic_memory : Immutable_bytes.t;
  static_memory : string;
  static_offset : int
}

let make dynamic static =
  {
    dynamic_memory = Immutable_bytes.make dynamic;
    static_memory = static;
    static_offset = String.length dynamic
  }

let read_byte memory address =
  if address < memory.static_offset then
    Immutable_bytes.read_byte memory.dynamic_memory address
  else
    int_of_char (memory.static_memory.[address - memory.static_offset])

let read_word memory address =
  let high = read_byte memory address in
  let low = read_byte memory (address + 1) in
  256 * high + low

let write_byte memory address value =
  if address >= memory.static_offset then
    failwith "attempt to write static memory"
  else
    let new_memory =
      Immutable_bytes.write_byte memory.dynamic_memory address value in
    { memory with dynamic_memory = new_memory }

let write_word memory address value =
  let w = ((value mod 65536) + 65536) mod 65536 in
  let high = w lsr 8 in
  let low = w land 0xFF in
  let first = write_byte memory address high in
  write_byte first (address + 1) low

let original memory =
  let original_bytes = Immutable_bytes.original memory.dynamic_memory in
  { memory with dynamic_memory = original_bytes }
