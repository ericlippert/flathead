(* Z-Machine tools written in OCaml, as part of my efforts to learn the language. *)

(* Debugging method to display bytes inside a file *)

let display_file_bytes filename start length =
    (* TODO: Use the version of input that fills in a mutable byte buffer. *)
    (* TODO: This is only available in OCaml 4.02, and I have 4.01 installed. *)
    let blocksize = 16 in
    let file = open_in_bin filename in
    seek_in file start;
    let rec print_loop i acc =
        if i = length then 
            acc
        else (
            let s = if i mod blocksize = 0 then Printf.sprintf "\n%06x: " (i + start) else "" in
            let b = input_byte file in
            let s2 = Printf.sprintf "%02x " b in
            print_loop (i + 1) (acc ^ s ^ s2)) in
    let result = (print_loop 0 "") ^ "\n" in
    close_in file;
    result;;

(* Debugging method to display bytes in a string *)

let display_string_bytes bytes start length =
    let blocksize = 16 in
    let rec print_loop i acc =
        if i = length then
            acc
        else (
            let s = if i mod blocksize = 0 then Printf.sprintf "\n%06x: " (i + start) else "" in
            let s2 = Printf.sprintf "%02x " (int_of_char bytes.[i + start]) in
            print_loop (i + 1) (acc ^ s ^ s2)) in
    (print_loop 0 "") ^ "\n";;

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

    (* *)   
    (* Debugging *)
    (* *)   

    let display_bytes story offset length =
        display_string_bytes story.raw_bytes offset length;;
        
    (* *)   
    (* Decoding memory *)
    (* *)   
    
    let fetch_bit n word =
        (word land (1 lsl n)) lsr n = 1;;
        
    let fetch_bits high length word =
        let mask = lnot (-1 lsl length) in
        (word lsr (high - length + 1)) land mask;;
        
    let read_byte_address story address = 
        read_ushort story.raw_bytes address;;
    
    let read_word story address = 
        read_ushort story.raw_bytes address;;
    
    let read_word_address story address =
        (read_ushort story.raw_bytes address) * 2;;
        
    let read_ubyte story address =
        read_ubyte story.raw_bytes address;;
        
    (* *)   
    (* Header *)
    (* *)   

    (* TODO: Header features beyond v3 *)
    
    let load_story filename = 
        { raw_bytes = read_entire_file filename };;
        
    let version_offset = 0;;
    let version story = 
        read_ubyte story version_offset;;

    (* TODO: Flags *)
        
    let high_memory_base_offset = 4;;
    let high_memory_base story =
        read_byte_address story high_memory_base_offset;;
        
    let initial_program_counter_offset = 6;;
    let initial_program_counter story =
        read_byte_address story initial_program_counter_offset;;

    let dictionary_base_offset = 8;;
    let dictionary_base story =
        read_byte_address story dictionary_base_offset;;
       
    let object_table_base_offset = 10;;
    let object_table_base story = 
        read_byte_address story object_table_base_offset;;
        
    let global_variables_table_base_offset = 12;;
    let global_variables_table_base story = 
        read_byte_address story global_variables_table_base_offset ;;
       
    let static_memory_base_offset = 14;;
    let static_memory_base story = 
        read_byte_address story static_memory_base_offset ;;

    (* TODO: Flags 2 *)
    
    let abbreviations_table_base_offset = 24;;
    let abbreviations_table_base story = 
        read_byte_address story abbreviations_table_base_offset ;;
        
    let display_header story =
        Printf.sprintf "Version                     : %d\n" (version story) ^
        Printf.sprintf "Abbreviations table base    : %04x\n" (abbreviations_table_base story) ^
        Printf.sprintf "Object table base           : %04x\n" (object_table_base story) ^
        Printf.sprintf "Global variables table base : %04x\n" (global_variables_table_base story) ^
        Printf.sprintf "Static memory base          : %04x\n" (static_memory_base story) ^
        Printf.sprintf "Dictionary base             : %04x\n" (dictionary_base story) ^
        Printf.sprintf "High memory base            : %04x\n" (high_memory_base story) ^
        Printf.sprintf "Initial program counter     : %04x\n" (initial_program_counter story);;
        
    (* *)   
    (* Abbreviation table and string decoding *)
    (* *)
    
    (* TODO: Assumes v3 abbreviation table *)
    let abbreviation_table_length = 96;;
    
    let abbreviation_address story n = 
        if n < 0 || n >= abbreviation_table_length then failwith "bad offset into abbreviation table";
        read_word_address story ((abbreviations_table_base story) + (n * 2));;
        
    type string_mode = 
        | Alphabet of int 
        | Abbreviation of int
        | Leading 
        | Trailing of int ;;
       
    let alphabet_table = [| 
        " "; "?"; "?"; "?"; "?"; "?"; "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; 
        "k"; "l"; "m"; "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"; 
        " "; "?"; "?"; "?"; "?"; "?"; "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; 
        "K"; "L"; "M"; "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W"; "X"; "Y"; "Z"; 
        " "; "?"; "?"; "?"; "?"; "?"; "?"; "\n"; "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; 
        "8"; "9"; "."; ","; "!"; "?"; "_"; "#"; "'"; "\""; "/"; "\\"; "-"; ":"; "("; ")" |];;
        
    (* gives the length in bytes of the encoded zstring, not the decoded string *)
    let zstring_length story address =
        let rec aux length current =
            if fetch_bit 15 (read_word story current) then length + 2
            else aux (length + 2) (current + 2) in
        aux 0 address;;
       
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
        10-bit character.  
        
        *)
        
        let process_zchar zchar mode =
            match (mode, zchar) with
            | (Alphabet _, 0) -> (" ", mode)
            | (Alphabet _, 1) -> ("", Abbreviation 0)
            | (Alphabet _, 2) -> ("", Abbreviation 32)
            | (Alphabet _, 3) -> ("", Abbreviation 64)
            | (Alphabet _, 4) -> ("", Alphabet 1)
            | (Alphabet _, 5) -> ("", Alphabet 2)
            | (Alphabet 2, 6) -> ("", Leading)
            | (Alphabet a, _) -> (alphabet_table.(a * 32 + zchar), Alphabet 0)
            | (Abbreviation a, _) -> (read_zstring story (abbreviation_address story (a + zchar)), Alphabet 0) 
            | (Leading, _) -> ("", (Trailing zchar)) 
            | (Trailing high, _) -> (String.make 1 (Char.chr (high * 32 + zchar)), Alphabet 0) in
         
        let rec aux mode1 current_address =
            let word = read_word story current_address in
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
        
    let display_zchar_bytes story offset length =
        let rec aux i acc =
            if i > length then acc
            else (
                let word = read_word story (offset + i) in
                let is_end = fetch_bits 15 1 word in
                let zchar1 = fetch_bits 14 5 word in
                let zchar2 = fetch_bits 9 5 word in
                let zchar3 = fetch_bits 4 5 word in
                let s = Printf.sprintf "(%01x %02x %02x %02x) " is_end zchar1 zchar2 zchar3 in
                aux (i + 2) (acc ^ s)) in
        aux 0 "";;
       
    let display_abbreviation_table story =
        let rec display_loop i acc =
            if i = abbreviation_table_length then acc
            else (
                let address = abbreviation_address story i in
                let value = read_zstring story address in
                let s = Printf.sprintf "%02x: %04x  %s\n" i address value in
                display_loop (i + 1) (acc ^ s)) in
        display_loop 0 "";;
        
    (* *)   
    (* Object table *)
    (* *)   
    
    (* TODO: 63 in version 4 and above *)
    let default_property_table_size = 31;;
    let default_property_table_entry_size = 2;;
    
    let default_property_table_base = object_table_base;;
    
    (* TODO: The spec implies that default properties
       are numbered starting at 1; is this right? *)
    let default_property_value story n =
        if n < 1 || n > default_property_table_size then failwith "invalid index into default property table"
        else  read_word story ((default_property_table_base story) + (n - 1) * default_property_table_entry_size);;
        
    let display_default_property_table story =
        let rec display_loop i acc =
            if i > default_property_table_size then acc
            else (
                let s = Printf.sprintf "%02x: %04x\n" i (default_property_value story i) in
                display_loop (i + 1) (acc ^ s)) in
        display_loop 1 "";;
        
    let object_tree_base story =
        (default_property_table_base story) + default_property_table_entry_size * default_property_table_size;;
         
    (* TODO: Object table entry is larger in version 4 *)
    let object_table_entry_size = 9;;    
    
    (* Oddly enough, the Z machine does not ever say how big the object table is. 
       Assume that the address of the first property block in the first object is
       the bottom of the object tree table. *)
       
    let object_attributes_word_1 story n = 
        read_word story ((object_tree_base story) + (n - 1) * object_table_entry_size);;
        
    let object_attributes_word_2 story n = 
        read_word story ((object_tree_base story) + (n - 1) * object_table_entry_size + 2);;
        
    let object_parent story n = 
        read_ubyte story ((object_tree_base story) + (n - 1) * object_table_entry_size + 4);;
    
    let object_sibling story n = 
        read_ubyte story ((object_tree_base story) + (n - 1) * object_table_entry_size + 5);;
        
    let object_child story n = 
        read_ubyte story ((object_tree_base story) + (n - 1) * object_table_entry_size + 6);;
        
    let object_property_address story n = 
        read_word story ((object_tree_base story) + (n - 1) * object_table_entry_size + 7);;
       
    let object_count story =
        ((object_property_address story 1) - (object_tree_base story)) / object_table_entry_size;;
       
    let object_name story n = 
        let length = read_ubyte story (object_property_address story n) in
        if length = 0 then "<unnamed>" 
        else read_zstring story ((object_property_address story n) + 1);;
        
    let property_addresses story object_number =
        let rec aux acc address =
            let b = read_ubyte story address in
            if b = 0 then 
                acc 
            else 
                let property_length = (fetch_bits 7 3 b) + 1 in
                let property_number = (fetch_bits 4 5 b) in
                aux ((property_number, property_length, address + 1) :: acc) (address + 1 + property_length) in
        let property_header_address = object_property_address story object_number in
        let property_name_word_length = read_ubyte story property_header_address in
        let first_property_address = property_header_address + 1 + property_name_word_length * 2 in
        aux [] first_property_address;;
            
    let display_properties story object_number =
        List.fold_left (fun s (property_number, length, address) -> s ^ (Printf.sprintf "%02x " property_number)) "" (property_addresses story object_number);; 
       
    let display_object_table story =
        let count = object_count story in 
        let rec display_loop i acc =
            if i > count then acc
            else (
                let flags1 = object_attributes_word_1 story i in
                let flags2 = object_attributes_word_2 story i in
                let parent = object_parent story i in
                let sibling = object_sibling story i in
                let child = object_child story i in
                let properties = object_property_address story i in
                let name = object_name story i in
                let s = (Printf.sprintf "%02x: %04x%04x %02x %02x %02x %04x %s " i flags1 flags2 parent sibling child properties name) ^
                    (display_properties story i) ^ "\n" in
                display_loop (i + 1) (acc ^ s)) in
        display_loop 1 "";;
        
    let null_object = 0;;
        
    let object_roots story =
        let rec aux i acc =
            if i < 1 then acc
            else aux (i -1) (if (object_parent story i) = null_object then (i :: acc) else acc) in
        aux (object_count story) [];;
       
    let display_object_tree story =
        let rec aux acc indent i =
            if i = null_object then acc 
            else (
                let o = (Printf.sprintf "%s %02x %s\n" indent i (object_name story i)) in
                let c = aux (acc ^ o) ("    " ^ indent) (object_child story i) in
                aux c indent (object_sibling story i)) in
        List.fold_left (fun s i -> s ^ (aux "" "" i)) "" (object_roots story);;
    
    (* *)   
    (* Dictionary *)
    (* *)   
    
    (* TODO: Only supports version 3 *)
    
    let word_separators_count story = 
        read_ubyte story (dictionary_base story);;
    
    let word_separators story = 
        let base = dictionary_base story in
        let count = read_ubyte story base in
        let rec aux acc i = 
            if i < 1 then acc 
            else aux ((read_ubyte story (base + i)) :: acc) (i - 1) in
        aux [] count;;
    
    let dictionary_entry_length story =
        read_ubyte story ((dictionary_base story) + (word_separators_count story) + 1);;
        
    let dictionary_entry_count story =     
        read_word story ((dictionary_base story) + (word_separators_count story) + 2);;
    
    let dictionary_table_base story =
        (dictionary_base story) + (word_separators_count story) + 4;;
        
    let dictionary_entry story dictionary_number =
        read_zstring story ((dictionary_table_base story) + dictionary_number * (dictionary_entry_length story));;
    
    let display_dictionary story =
        let entry_count = dictionary_entry_count story in 
        let s = (Printf.sprintf "Separator count: %d\n" (word_separators_count story)) ^
        (Printf.sprintf "Entry length:    %d\n" (dictionary_entry_length story)) ^
        (Printf.sprintf "Entry count:     %d\n" entry_count) in
        let rec display_loop i acc =
            if i >= entry_count then acc
            else (
                let r = (Printf.sprintf "%04x: %s\n" i (dictionary_entry story i)) in
                display_loop (i + 1) (acc ^ r) ) in
        display_loop 0 s;;

(* *)
(* Bytecode *)
(* *)


(* TODO: Extended *)
type opcode_form =
    | Long_form
    | Short_form
    | Variable_form;;

type operand_count =
    | OP0
    | OP1
    | OP2
    | VAR;;

type variable_location =
    | Stack
    | Local of int
    | Global of int;;
    
type operand_type =
    | Large_operand
    | Small_operand
    | Variable_operand
    | Omitted;;
    
type operand =
    | Large of int
    | Small of int
    | Variable of variable_location;;

type bytecode = 
              | OP2_1   | OP2_2   | OP2_3   | OP2_4   | OP2_5   | OP2_6   | OP2_7
    | OP2_8   | OP2_9   | OP2_10  | OP2_11  | OP2_12  | OP2_13  | OP2_14  | OP2_15
    | OP2_16  | OP2_17  | OP2_18  | OP2_19  | OP2_20  | OP2_21  | OP2_22  | OP2_23
    | OP2_24  | OP2_25  | OP2_26  | OP2_27  | OP2_28
    | OP1_128 | OP1_129 | OP1_130 | OP1_131 | OP1_132 | OP1_133 | OP1_134 | OP1_135  
    | OP1_136 | OP1_137 | OP1_138 | OP1_139 | OP1_140 | OP1_141 | OP1_142 | OP1_143  
    | OP0_176 | OP0_177 | OP0_178 | OP0_179 | OP0_180 | OP0_181 | OP0_182 | OP0_183 
    | OP0_184 | OP0_185 | OP0_186 | OP0_187 | OP0_188 | OP0_189 | OP0_190 | OP0_191 
    | VAR_224 | VAR_225 | VAR_226 | VAR_227 | VAR_228 | VAR_229 | VAR_230 | VAR_231
    | VAR_232 | VAR_233 | VAR_234 | VAR_235 | VAR_236 | VAR_237 | VAR_238 | VAR_239
    | VAR_240 | VAR_241 | VAR_242 | VAR_243 | VAR_244 | VAR_245 | VAR_246 | VAR_247
    | VAR_248 | VAR_249 | VAR_250 | VAR_251 | VAR_252 | VAR_253 | VAR_254 | VAR_255
    | ILLEGAL;;
   
(* The tables which follow are maps from the opcode identification number
   to the opcode type; the exact order matters. *)
    
let one_operand_bytecodes = [|
    OP1_128; OP1_129; OP1_130; OP1_131; OP1_132; OP1_133; OP1_134; OP1_135;
    OP1_136; OP1_137; OP1_138; OP1_139; OP1_140; OP1_141; OP1_142; OP1_143  |];;
    
let zero_operand_bytecodes = [| 
    OP0_176; OP0_177; OP0_178; OP0_179; OP0_180; OP0_181; OP0_182; OP0_183;
    OP0_184; OP0_185; OP0_186; OP0_187; OP0_188; OP0_189; OP0_190; OP0_191  |];;
    
let two_operand_bytecodes =[|
    ILLEGAL; OP2_1;  OP2_2;  OP2_3;  OP2_4;  OP2_5;   OP2_6;   OP2_7;
    OP2_8;   OP2_9;  OP2_10; OP2_11; OP2_12; OP2_13;  OP2_14;  OP2_15;
    OP2_16;  OP2_17; OP2_18; OP2_19; OP2_20; OP2_21;  OP2_22;  OP2_23;
    OP2_24;  OP2_25; OP2_26; OP2_27; OP2_28; ILLEGAL; ILLEGAL; ILLEGAL |];;
   
let var_operand_bytecodes = [|
    VAR_224; VAR_225; VAR_226; VAR_227; VAR_228; VAR_229; VAR_230; VAR_231;
    VAR_232; VAR_233; VAR_234; VAR_235; VAR_236; VAR_237; VAR_238; VAR_239;
    VAR_240; VAR_241; VAR_242; VAR_243; VAR_244; VAR_245; VAR_246; VAR_247;
    VAR_248; VAR_249; VAR_250; VAR_251; VAR_252; VAR_253; VAR_254; VAR_255 |];;

let opcode_name opcode = 
    match opcode with
    | ILLEGAL -> "ILLEGAL"
    | OP2_1   -> "je"
    | OP2_2   -> "jl"
    | OP2_3   -> "jg"
    | OP2_4   -> "dec_chk"
    | OP2_5   -> "inc_chk"
    | OP2_6   -> "jin"
    | OP2_7   -> "test"
    | OP2_8   -> "or"
    | OP2_9   -> "and"
    | OP2_10  -> "test_attr"
    | OP2_11  -> "set_attr"
    | OP2_12  -> "clear_attr"
    | OP2_13  -> "store"
    | OP2_14  -> "insert_obj"
    | OP2_15  -> "get_prop_addr"
    | OP2_16  -> "get_next_prop"
    | OP2_17  -> "get_prop"
    | OP2_18  -> "get_prop_addr"
    | OP2_19  -> "get_next_prop"
    | OP2_20  -> "add"
    | OP2_21  -> "sub"
    | OP2_22  -> "mul"
    | OP2_23  -> "div"
    | OP2_24  -> "mod"
    | OP2_25  -> "call_2s"
    | OP2_26  -> "call_2n"
    | OP2_27  -> "set_colour"
    | OP2_28  -> "throw"
    | OP1_128 -> "jz"
    | OP1_129 -> "get_sibling"
    | OP1_130 -> "get_child"
    | OP1_131 -> "get_parent"
    | OP1_132 -> "get_prop_len"
    | OP1_133 -> "inc"
    | OP1_134 -> "dec"
    | OP1_135 -> "print_addr"
    | OP1_136 -> "call_1s"
    | OP1_137 -> "remove_obj"
    | OP1_138 -> "print_obj"
    | OP1_139 -> "ret"
    | OP1_140 -> "jump"
    | OP1_141 -> "print_paddr"
    | OP1_142 -> "load"
    | OP1_143 -> "not"
    | OP0_176 -> "rtrue"
    | OP0_177 -> "rfalse"
    | OP0_178 -> "print"
    | OP0_179 -> "print_ret"
    | OP0_180 -> "nop"
    | OP0_181 -> "save"
    | OP0_182 -> "restore"
    | OP0_183 -> "restart"
    | OP0_184 -> "ret_popped"
    | OP0_185 -> "pop"
    | OP0_186 -> "quit"
    | OP0_187 -> "new_line"
    | OP0_188 -> "show_status"
    | OP0_189 -> "verify"
    | OP0_190 -> "EXTENDED TODO"
    | OP0_191 -> "piracy"
    | VAR_224 -> "call"
    | VAR_225 -> "storew"
    | VAR_226 -> "storeb"
    | VAR_227 -> "put_prop"
    | VAR_228 -> "sread"
    | VAR_229 -> "print_char"
    | VAR_230 -> "print_num"
    | VAR_231 -> "random"
    | VAR_232 -> "push"
    | VAR_233 -> "pull"
    | VAR_234 -> "split_window"
    | VAR_235 -> "set_window"
    | VAR_236 -> "call_vs2"
    | VAR_237 -> "erase_window"
    | VAR_238 -> "erase_line"
    | VAR_239 -> "set_cursor"
    | VAR_240 -> "get_cursor"
    | VAR_241 -> "set_text_style"
    | VAR_242 -> "buffer_mode"
    | VAR_243 -> "output_stream"
    | VAR_244 -> "input_stream"
    | VAR_245 -> "sound_effect"
    | VAR_246 -> "read_char"
    | VAR_247 -> "scan_table"
    | VAR_248 -> "not"
    | VAR_249 -> "call_vn"
    | VAR_250 -> "call_vn2"
    | VAR_251 -> "tokenise"
    | VAR_252 -> "encode_text"
    | VAR_253 -> "copy_table"
    | VAR_254 -> "print_table"
    | VAR_255 -> "check_arg_count";;
    
type instruction =
{
    opcode : bytecode;
    address : int;
    length : int;
    operands : operand list;
    store : variable_location option;
    branch : (bool * int) option;
    text : string option;
};;

let decode_instruction story address =

    let has_branch opcode = 
        match opcode with
        | OP2_1 
        | OP2_2 
        | OP2_3 
        | OP2_4 
        | OP2_5 
        | OP2_6 
        | OP2_7 
        | OP2_10 
        | OP1_128
        | OP1_129
        | OP1_130
        | OP0_181 
        | OP0_182  
        | OP0_189 
        | OP0_191
        | VAR_247
        | VAR_255 -> true
        | _ -> false in

    let has_store opcode =
        match opcode with
        | OP2_8
        | OP2_9
        | OP2_15
        | OP2_16
        | OP2_17
        | OP2_18
        | OP2_19
        | OP2_20
        | OP2_21
        | OP2_22
        | OP2_23
        | OP2_24
        | OP2_25
        | OP1_129
        | OP1_130
        | OP1_131
        | OP1_132
        | OP1_136
        | OP1_142
        | OP1_143
        | VAR_224
        | VAR_231
        | VAR_236
        | VAR_246
        | VAR_247
        | VAR_248 -> true
        | _ -> false in

    let has_text opcode =
        match opcode with
        | OP0_178
        | OP0_179 -> true
        | _ -> false in
   
    let decode_types n = 
        match n with 
        | 0 -> Large_operand
        | 1 -> Small_operand
        | 2 -> Variable_operand
        | _ -> Omitted in
        
    let decode_variable_types types = 
        let rec aux i acc =
            if i > 3 then acc
            else 
                match decode_types (fetch_bits (i * 2 + 1) 2 types) with
                | Omitted -> aux (i + 1) acc
                | x -> aux (i + 1) (x :: acc) in
        aux 0 [] in
        
    let decode_variable n =
        if n = 0 then Stack
        else if n < 0x10 then Local n
        else Global n in
        
    let rec decode_operands operand_address operand_types =
        match operand_types with
        | [] -> []
        | Large_operand :: types -> (Large (read_word story operand_address))  :: (decode_operands (operand_address + 2) types)
        | Small_operand :: types -> (Small (read_ubyte story operand_address))  :: (decode_operands (operand_address + 1) types)
        | Variable_operand :: types -> (Variable (decode_variable (read_ubyte story operand_address))) :: (decode_operands (operand_address + 1) types)
        | Omitted :: _ -> failwith "omitted operand type passed to decode operands" in
    
    let rec operand_size operand_types =
        match operand_types with
        | [] -> 0
        | Large_operand :: types -> 2 + (operand_size types)
        | _ :: types -> 1 + (operand_size types) in
        
    (* Spec 4.7 *)
    
    let branch_size branch_address =
        let b = (read_ubyte story branch_address) in 
        if (fetch_bit 6 b) then 1 else 2 in
    
    let decode_branch branch_address =
        let b = (read_ubyte story branch_address) in 
        let sense = (fetch_bit 7 b) in
        let bottom6 = fetch_bits 5 6 b in
        if (fetch_bit b 6) then 
            (sense, bottom6)
        else
            let unsigned = 256 * bottom6 + (read_ubyte story (branch_address + 1)) in
            if unsigned < 8192 then (sense, unsigned) else (sense, unsigned - 16384) in
        
    let b = read_ubyte story address in
    
    let form = match fetch_bits 7 2 b with
    | 3 -> Variable_form
    | 2 -> Short_form
    | _ -> Long_form in
    
    let op_count = match form with
    | Variable_form -> if fetch_bit 5 b then VAR else OP2
    | Long_form -> OP2
    | Short_form -> if fetch_bits 5 2 b = 3 then OP0 else OP1 in
    
    let opcode = match op_count with
    | OP0 -> zero_operand_bytecodes.(fetch_bits 3 4 b)
    | OP1 -> one_operand_bytecodes.(fetch_bits 3 4 b) 
    | OP2 -> two_operand_bytecodes.(fetch_bits 4 5 b) 
    | VAR -> var_operand_bytecodes.(fetch_bits 4 5 b) in
    
    let opcode_length = 1 in
    
    let operand_types = match form with
    | Short_form -> 
        (match op_count with 
        | OP0 -> [] 
        | _ -> [decode_types (fetch_bits 5 2 b)])
    | Long_form -> 
        (match fetch_bits 6 2 b with
        | 0 -> [ Small_operand; Small_operand ]
        | 1 -> [ Small_operand; Variable_operand ]
        | 2 -> [ Variable_operand; Small_operand ]
        | _ -> [ Variable_operand; Variable_operand ])
    | Variable_form -> decode_variable_types (read_ubyte story (address + opcode_length)) in
    
    let type_length = (match form with Variable_form -> 1 | _ -> 0) in
    
    let operands = decode_operands (address + opcode_length + type_length) operand_types in
    
    let operand_length = operand_size operand_types in
    
    let store_address = address + opcode_length + type_length + operand_length in
      
    let store = 
        if has_store opcode then Some (decode_variable (read_ubyte story store_address))
        else None in
    
    let store_length = if has_store opcode then 1 else 0 in
    
    let branch_address = store_address + store_length in
    
    let branch = 
        if has_branch opcode then Some (decode_branch branch_address)
        else None in
        
    let branch_length = if has_branch opcode then (branch_size branch_address) else 0 in
    
    let text_address = branch_address + branch_length in
    
    let text = 
        if has_text opcode then Some (read_zstring story text_address) else None in
        
    let text_length = if has_text opcode then (zstring_length story text_address) else 0 in
    
    let total_length = opcode_length + type_length + operand_length + store_length + branch_length + text_length in
    
    {opcode; address; length = total_length; operands; store; branch; text};;
    
    let display_operand operand =
        match operand with
        | Large large -> Printf.sprintf "%04x " large
        | Small small -> Printf.sprintf "%02x " small
        | Variable Stack -> "stack "
        | Variable Local local -> Printf.sprintf "local%02x " local 
        | Variable Global global -> Printf.sprintf "global%02x " global;;
        
    let display_operands operands = 
        List.fold_left (fun acc operand -> acc ^ (display_operand operand)) "" operands;;

    let display_instruction instr =
        Printf.sprintf "%04x-%04x: %s %s\n" instr.address (instr.address + instr.length - 1) (opcode_name instr.opcode) (display_operands instr.operands);;
        
    let display_instructions story address count =
        let rec aux acc addr c =
            if c = 0 then acc
            else (
                let instr = decode_instruction story addr in
                let s = display_instruction instr in
                aux (acc  ^ s) (addr + instr.length) (c - 1)) in
        aux "" address count;;
end

open Story;;

let s = load_story "ZORK1.DAT";;
print_endline (display_header s);
print_endline (display_bytes s (0x4f7e) 64);; 
print_endline (display_instructions s (initial_program_counter s) 50);;

(*
print_endline (display_abbreviation_table s);;
print_endline (display_default_property_table s);; 
print_endline (display_object_table s);;
print_endline (display_object_tree s);;
print_endline (display_dictionary s);;  

*)
