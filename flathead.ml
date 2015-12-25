(* Z-Machine tools written in OCaml, as part of my efforts to learn the language. *)

(* Debugging method to display bytes inside a file *)

let display_file_bytes filename start length =
    (* TODO: Use the version of input that fills in a mutable byte buffer. *)
    (* TODO: This is only available in OCaml 4.02, and I have 4.01 installed. *)
    let blocksize = 16 in
    let file = open_in_bin filename in
    seek_in file start;
    let rec print_loop i =
        if i = length then 
            ()
        else (
            if i mod blocksize = 0 then Printf.printf "\n%06x: " (i + start);
            let b = input_byte file in
            Printf.printf "%02x " b;
            print_loop (i + 1)) in
    print_loop 0; 
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

    (* Debugging *)

    let display_bytes story offset length =
        display_string_bytes story.raw_bytes offset length;;

    (* Decoding memory *)
    
    let fetch_bit n word =
        (word land (1 lsl n)) lsr n = 1;;
        
    let fetch_bits high length word =
        let mask = lnot (-1 lsl length) in
        (word lsr (high - length + 1)) land mask;;
        
    let read_byte_address = read_ushort;;
    
    let read_word = read_ushort;;
    
    let read_word_address bytes n =
        (read_ushort bytes n) * 2;;
        
    (* Header *)

    (* TODO: Header features beyond v3 *)
    
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
        
    (* Abbreviation table and string decoding *)
    
    (* TODO: Assumes v3 abbreviation table *)
    let abbreviation_table_length = 96;;
    
    let abbreviation_address story n = 
        if n < 0 || n >= abbreviation_table_length then failwith "bad offset into abbreviation table";
        read_word_address story.raw_bytes ((abbreviations_table_base story) + (n * 2));;
        
    type string_mode = Alphabet of int | Abbreviation of int;;
       
    let alphabet_table = [| 
        " "; "?"; "?"; "?"; "?"; "?"; "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; 
        "k"; "l"; "m"; "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"; 
        " "; "?"; "?"; "?"; "?"; "?"; "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; 
        "K"; "L"; "M"; "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W"; "X"; "Y"; "Z"; 
        " "; "?"; "?"; "?"; "?"; "?"; "?"; "\n"; "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; 
        "8"; "9"; "."; ","; "!"; "?"; "_"; "#"; "'"; "\""; "/"; "\\"; "-"; ":"; "("; ")" |];;
       
    let rec read_zstring story address =
        (* TODO: Only processes version 3 strings *)
        
        (* zstrings encode three characters into two-byte words.
        
        The high bit is the end-of-string marker, followed by three
        five-bit zchars.
        
        The meaning of the next zchar(s) depends on the current.
        
        If the current zchar is 1, 2 or 3 then the next is an offset
        into the abbreviation table; fetch the string indicated there.
        
        If the current zchar is 4 or 5 then the next is an offset into the
        uppercase or punctuation alphabets, except if the current is 5
        and the next is 6. In that case the two zchars following are a single
        10-bit character. (TODO: Not implemented)
        
        *)
        
        let process_zchar zchar mode =
            match (mode, zchar) with
            | (Alphabet _, 0) -> (" ", mode)
            | (Alphabet _, 1) -> ("", Abbreviation 0)
            | (Alphabet _, 2) -> ("", Abbreviation 32)
            | (Alphabet _, 3) -> ("", Abbreviation 64)
            | (Alphabet _, 4) -> ("", Alphabet 1)
            | (Alphabet _, 5) -> ("", Alphabet 2)
            | (Alphabet 2, 6) -> failwith "TODO: Multi-byte characters not yet implemented"
            | (Alphabet a, n) -> (alphabet_table.(a * 32 + n), Alphabet 0)
            | (Abbreviation a, n) -> (read_zstring story (abbreviation_address story (a + n)), Alphabet 0) in
         
        let rec aux mode1 current_address =
            let word = read_word story.raw_bytes current_address in
            let is_end = fetch_bit 15 word in
            let zchar1 = fetch_bits 14 5 word in
            let zchar2 = fetch_bits 9 5 word in
            let zchar3 = fetch_bits 4 5 word in
            let (text1, mode2) = process_zchar zchar1 mode1 in
            let (text2, mode3) = process_zchar zchar2 mode2 in
            let (text3, mode_next) = process_zchar zchar3 mode3 in
            let text_next = if is_end then "" else aux mode_next (current_address + 2) in
            text1 ^ text2 ^ text3 ^ text_next in
            
        aux (Alphabet 0) address;;
       
    let display_abbreviation_table story =
        let rec display_loop i =
            if i = abbreviation_table_length then ()
            else (
                let address = abbreviation_address story i in
                let value = read_zstring story address in
                Printf.printf "%02x: %04x  %s\n" i address value;
                display_loop (i + 1)) in
        display_loop 0;;
        
        
    (* Object table *)
    
    (* TODO: 63 in version 4 and above *)
    let default_property_table_size = 31;;
    let default_property_table_entry_size = 2;;
    
    let default_property_table_base = object_table_base;;
    
    (* TODO: The spec implies that default properties
       are numbered starting at 1; is this right? *)
    let default_property_value story n =
        if n < 1 || n > default_property_table_size then failwith "invalid index into default property table"
        else  read_word story.raw_bytes ((default_property_table_base story) + (n - 1) * default_property_table_entry_size);;
        
    let display_default_property_table story =
        let rec display_loop i =
            if i > default_property_table_size then ()
            else (
                Printf.printf "%02x: %04x\n" i (default_property_value story i);
                display_loop (i + 1)) in
        display_loop 1;;
        
    let object_tree_base story =
        (default_property_table_base story) + default_property_table_entry_size * default_property_table_size;;
         
    (* TODO: Object table entry is larger in version 4 *)
    let object_table_entry_size = 9;;    
    
    (* Oddly enough, the Z machine does not ever say how big the object table is. 
       Assume that the address of the first property block in the first object is
       the bottom of the object tree table. *)
       
    let object_attributes_word_1 story n = 
        read_word story.raw_bytes ((object_tree_base story) + (n - 1) * object_table_entry_size);;
        
    let object_attributes_word_2 story n = 
        read_word story.raw_bytes ((object_tree_base story) + (n - 1) * object_table_entry_size + 2);;
        
    let object_parent story n = 
        read_ubyte story.raw_bytes ((object_tree_base story) + (n - 1) * object_table_entry_size + 4);;
    
    let object_sibling story n = 
        read_ubyte story.raw_bytes ((object_tree_base story) + (n - 1) * object_table_entry_size + 5);;
        
    let object_child story n = 
        read_ubyte story.raw_bytes ((object_tree_base story) + (n - 1) * object_table_entry_size + 6);;
        
    let object_property_address story n = 
        read_word story.raw_bytes ((object_tree_base story) + (n - 1) * object_table_entry_size + 7);;
       
    let object_count story =
        ((object_property_address story 1) - (object_tree_base story)) / object_table_entry_size;;
       
    let object_name story n = 
        let length = read_ubyte story.raw_bytes (object_property_address story n) in
        if length = 0 then "<unnamed>" 
        else read_zstring story ((object_property_address story n) + 1);;
       
    let display_object_table story =
        let count = object_count story in 
        let rec display_loop i =
            if i > count then ()
            else (
                let flags1 = object_attributes_word_1 story i in
                let flags2 = object_attributes_word_2 story i in
                let parent = object_parent story i in
                let sibling = object_sibling story i in
                let child = object_child story i in
                let properties = object_property_address story i in
                let name = object_name story i in
                Printf.printf "%02x: %04x%04x %02x %02x %02x %04x %s\n" i flags1 flags2 parent sibling child properties name;
                display_loop (i + 1)) in
        display_loop 1;;
        
    let null_object = 0;;
        
    let object_roots story =
        let rec aux i acc =
            if i < 1 then acc
            else aux (i -1) (if (object_parent story i) = null_object then (i :: acc) else acc) in
        aux (object_count story) [];;
       
    let display_object_tree story =
        let rec aux indent i =
            if i = null_object then () 
            else (
                Printf.printf "%s %02x %s\n" indent i (object_name story i);
                aux ("    " ^ indent) (object_child story i);
                aux indent (object_sibling story i)) in
        List.iter (aux "") (object_roots story);;
          
end

let s = Story.load_story "ZORK1.DAT";;
Story.display_header s;
(* Story.display_bytes s (Story.object_tree_base s) 64;; *)
(* Story.display_abbreviation_table s;; *)
(* Story.display_default_property_table s;; *)
Story.display_object_tree s;;
