(* Z-Machine tools written in OCaml, as part of my efforts to learn the language. *)

(* Debugging method to display bytes inside a file *)

let display_file_bytes filename start length =
    (* TODO: Use the version of input that fills in a mutable byte buffer. *)
    (* TODO: This is only available in OCaml 4.02, and I have 4.01 installed. *)
    let blocksize = 16 in
    let file = open_in_bin filename in
    seek_in file start;
    for i = 0 to (length - 1) do
        if i mod blocksize = 0 then Printf.printf "\n%06x: " (i + start);
        let b = input_byte file in
        Printf.printf "%02x " b;
    done;
    Printf.printf "\n";
    close_in file;;

(* Debugging method to display bytes in a string *)

let display_string_bytes bytes start length =
    let blocksize = 16 in
    for i = 0 to (length - 1) do
        if i mod blocksize = 0 then Printf.printf "\n%06x: " (i + start);
        Printf.printf "%02x " (int_of_char bytes.[i + start]);
    done;
    Printf.printf "\n";;

(* Takes a file name and produces a string containing the whole binary file. *)

let read_entire_file filename =
    (* TODO: Use the version that reads into a mutable byte buffer instead
       TODO: of a string, when you get OCaml 4.02 *)
       
    let file = open_in_bin filename in
    let length = in_channel_length file in
    let bytes = String.create length in
    really_input file bytes 0 length;
    close_in file;
    bytes;;
    
(* Reads an unsigned byte from a string *)

let read_ubyte bytes offset =
    int_of_char bytes.[offset];;
    
(* Reads an unsigned 16 bit integer from a string *)
    
let read_ushort bytes offset =
    (* two-byte integers are stored in high / low order *)
    (int_of_char bytes.[offset]) * 256 + (int_of_char bytes.[offset + 1]);;
    
(* Reads a signed 16 bit integer from a string *)

let read_short bytes offset =
    let value = read_ushort bytes offset in
    if value > 32767 then value - 65536 else value;;
   
module Story = struct
    type t =
    {
        raw_bytes : string
    };;

    (* TODO: Header features beyond v3 *)
    
    let read_byte_address = read_ushort;;

    let load_story filename = 
        { raw_bytes = read_entire_file filename };;
        
    let version_offset = 0;;
    let version story = 
        read_ubyte story.raw_bytes version_offset;;

    (* TODO: Flags *)
        
    let high_memory_base_offset = 4;;
    let high_memory_base story =
        read_byte_address story.raw_bytes high_memory_base_offset;;
        
    let initial_program_counter_offset = 6;;
    let initial_program_counter story =
        read_byte_address story.raw_bytes initial_program_counter_offset;;

    let dictionary_base_offset = 8;;
    let dictionary_base story =
        read_byte_address story.raw_bytes dictionary_base_offset;;
       
    let object_table_base_offset = 10;;
    let object_table_base story = 
        read_byte_address story.raw_bytes object_table_base_offset;;
        
    let global_variables_table_base_offset = 12;;
    let global_variables_table_base story = 
        read_byte_address story.raw_bytes global_variables_table_base_offset ;;
       
    let static_memory_base_offset = 14;;
    let static_memory_base story = 
        read_byte_address story.raw_bytes static_memory_base_offset ;;

    (* TODO: Flags 2 *)
    
    let abbreviations_table_base_offset = 24;;
    let abbreviations_table_base story = 
        read_byte_address story.raw_bytes abbreviations_table_base_offset ;;
    
    let display_header story =
        Printf.printf "Version                     : %d\n" (version story);
        Printf.printf "Abbreviations table base    : %04x\n" (abbreviations_table_base story);
        Printf.printf "Object table base           : %04x\n" (object_table_base story);
        Printf.printf "Global variables table base : %04x\n" (global_variables_table_base story);
        Printf.printf "Static memory base          : %04x\n" (static_memory_base story);
        Printf.printf "Dictionary base             : %04x\n" (dictionary_base story);
        Printf.printf "High memory base            : %04x\n" (high_memory_base story);
        Printf.printf "Initial program counter     : %04x\n" (initial_program_counter story);
        ;;
        
    let display_bytes story offset length =
        display_string_bytes story.raw_bytes offset length;;
        
end

let s = Story.load_story "ZORK1.DAT";;
Story.display_header s;
