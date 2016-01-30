open Type
open Utility


let () = 
    let addr1 = Byte_address 1 in
    let bytes_a = Immutable_bytes.make "Hello world" in
    let bytes_b = Immutable_bytes.write_byte bytes_a addr1 65 in
    let b_a = Immutable_bytes.read_byte bytes_a addr1 in
    let b_b = Immutable_bytes.read_byte bytes_b addr1 in
    Printf.printf "%d %d\n" b_a b_b

