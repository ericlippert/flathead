(* Z-Machine tools written in OCaml, as part of my efforts to learn the language. *)

(* Helper method that takes an item and a function that produces related items.
   The result is the transitive closure of the relation. *)
   
let transitive_closure_many items relation =
    let rec merge related set stack = 
        match related with
        | [] -> (set, stack)
        | head :: tail -> 
            if List.mem head set then merge tail set stack
            else merge tail (head :: set) (head :: stack) in
    let rec aux set stack =
        match stack with
        | [] -> set
        | head :: tail -> 
            let (new_set, new_stack) = merge (relation head) set tail in
            aux new_set new_stack in
    aux [] items;;
    
let transitive_closure item relation =
    transitive_closure_many [item] relation;;

let reflexive_closure_many items relation =
    let t = transitive_closure_many items relation in
    List.fold_left (fun s i -> if List.mem i s then s else i :: s) t items;;

let reflexive_closure item relation = 
    reflexive_closure_many [item] relation;;

(* Takes a file name and produces a string containing the whole binary file. *)

let read_entire_file filename =
    let file = open_in_bin filename in
    let length = in_channel_length file in
    let bytes = String.create length in
    really_input file bytes 0 length;
    close_in file;
    bytes;;
   
  
let unsigned_word word = 
    ((word mod 65536) + 65536) mod 65536;;

let signed_word word = 
    let canonical = unsigned_word word in
    if canonical > 32767 then canonical - 65536 else canonical;;   
    
module IntMap = Map.Make(struct type t = int let compare = compare end)
     
module ImmutableBytes = struct

    (* TODO: Track number of edits; when size of tree exceeds *)
    (* TODO: size of a new string, consider making a new string. *)
    
    (* TODO: Consider: is it worthwhile to make a tree of int32s or int64s
             instead of chars? The total memory consumed by all the nodes
             would be smaller. *)


    type t = 
    {
        original_bytes : string;
        edits : char IntMap.t 
    };;
    
    let make bytes = { original_bytes = bytes; edits = IntMap.empty };;
    
    let read_ubyte bytes address =
        let c = 
            if IntMap.mem address bytes.edits then IntMap.find address bytes.edits
            else  bytes.original_bytes.[address] in
        int_of_char c;;
        
    let write_ubyte bytes address value = 
        let ubyte_of_int value = 
            ((value mod 256) + 256 ) mod 256 in
        let b = char_of_int (ubyte_of_int value) in
        { bytes with edits = IntMap.add address b bytes.edits };;
    
end

module Memory = struct

    type t = 
    {
        dynamic_memory : ImmutableBytes.t;
        static_memory : string;
        static_offset : int
    };;
    
    let make dynamic static =
        { dynamic_memory = ImmutableBytes.make dynamic; static_memory = static; static_offset = String.length dynamic };;
    
    let read_ubyte memory address =
        if address < memory.static_offset then ImmutableBytes.read_ubyte memory.dynamic_memory address
        else int_of_char (memory.static_memory.[address - memory.static_offset]);;
        
    let read_ushort memory address =
        let high = read_ubyte memory address in
        let low = read_ubyte memory (address + 1) in
        256 * high + low;;
        
    let read_short memory address = 
        signed_word (read_ushort memory address);;
        
    let ushort_of_int value = 
        ((value mod 65536) + 65536 ) mod 65536;;
        
    let write_ubyte memory address value = 
        if address >= memory.static_offset then failwith "attempt to write static memory"
        else { memory with dynamic_memory = ImmutableBytes.write_ubyte memory.dynamic_memory address value };;
        
    let write_ushort memory address value = 
        let w = ushort_of_int value in
        let high = w lsr 8 in
        let low = w land 0xFF in
        let first = write_ubyte memory address high in
        write_ubyte first (address + 1) low;;
       
    let display_bytes memory address length =
        let blocksize = 16 in
        let rec print_loop i acc =
            if i = length then
                acc
            else (
                let s = if i mod blocksize = 0 then Printf.sprintf "\n%06x: " (i + address) else "" in
                let s2 = Printf.sprintf "%02x " (read_ubyte memory (i + address)) in
            print_loop (i + 1) (acc ^ s ^ s2)) in
        (print_loop 0 "") ^ "\n";;
end
   
module Story = struct
    type t =
    {
        memory : Memory.t
    };;

    (* *)   
    (* Debugging *)
    (* *)   

    let display_bytes story address length =
        Memory.display_bytes story.memory address length;;
        
    (* *)   
    (* Decoding memory *)
    (* *)   
    
    let fetch_bit n word =
        (word land (1 lsl n)) lsr n = 1;;
        
    let clear_bit n word = 
        word land (lnot (1 lsl n));;
        
    let set_bit n word = 
        word lor (1 lsl n);;
        
    let fetch_bits high length word =
        let mask = lnot (-1 lsl length) in
        (word lsr (high - length + 1)) land mask;;
        
    let read_byte_address story address = 
        Memory.read_ushort story.memory address;;
    
    let read_word story address = 
        Memory.read_ushort story.memory address;;
    
    let read_word_address story address =
        (Memory.read_ushort story.memory address) * 2;;
        
    let read_ubyte story address =
        Memory.read_ubyte story.memory address;;
        
    let write_word story address value =
        { memory = Memory.write_ushort story.memory address value };;
        
    let write_byte story address value =
        { memory = Memory.write_ubyte story.memory address value };;

    let write_string story address text =
        let length = String.length text in
        let rec aux i s =
            if i = length then s 
            else aux (i + 1) (write_byte s (address + i) (int_of_char text.[i])) in
        let copied = aux 0 story in
        write_byte copied (address + length) 0;;
        
    (* *)   
    (* Header *)
    (* *)   

    (* TODO: Header features beyond v3 *)
    
    let header_size = 64;;     
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
        
    let load_story filename = 
        (* TODO: Could read in just the header first, then the dynamic block as a string,
        then the static block as a string. Less copying that way. *)
        
        let file = read_entire_file filename in
        let len = String.length file in
        if len < header_size then failwith (Printf.sprintf "%s is not a valid story file" filename);
        let version = int_of_char file.[version_offset] in
        if version <> 3 then failwith (Printf.sprintf "%s is not a valid version 3 story file" filename);
        let high = int_of_char file.[static_memory_base_offset] in
        let low = int_of_char file.[static_memory_base_offset + 1] in
        let dynamic_length = high * 256 + low in
        if (dynamic_length > len) then failwith (Printf.sprintf "%s is not a valid story file" filename);
        let dynamic = String.sub file 0 dynamic_length in
        let static = String.sub file dynamic_length (len - dynamic_length) in
        { memory = Memory.make dynamic static };;
        
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
                let s = Printf.sprintf "%04x(%01x %02x %02x %02x) " word is_end zchar1 zchar2 zchar3 in
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
        
    let attribute_count = 32;;
    (* TODO: 48 attributes in version 4 *)
    
    let object_attribute story object_number attribute_number =
        if attribute_number < 0 || attribute_number >= attribute_count then failwith "bad attribute";
        let offset = attribute_number / 8 in
        let address = (object_tree_base story) + (object_number - 1) * object_table_entry_size + offset in
        let byte = read_ubyte story address in
        let bit = 7 - (attribute_number mod 8) in
        fetch_bit bit byte;;
        
    let set_object_attribute story object_number attribute_number = 
        if attribute_number < 0 || attribute_number >= attribute_count then failwith "bad attribute";
        let offset = attribute_number / 8 in
        let address = (object_tree_base story) + (object_number - 1) * object_table_entry_size + offset in
        let byte = read_ubyte story address in
        let bit = 7 - (attribute_number mod 8) in
        let result = set_bit bit byte in
        write_byte story address result;;
        
    let clear_object_attribute story object_number attribute_number = 
        if attribute_number < 0 || attribute_number >= attribute_count then failwith "bad attribute";
        let offset = attribute_number / 8 in
        let address = (object_tree_base story) + (object_number - 1) * object_table_entry_size + offset in
        let byte = read_ubyte story address in
        let bit = 7 - (attribute_number mod 8) in
        let result = clear_bit bit byte in
        write_byte story address result;;
        
    let object_parent story obj = 
        read_ubyte story ((object_tree_base story) + (obj - 1) * object_table_entry_size + 4);;
    
    let set_object_parent story obj new_parent = 
        write_byte story ((object_tree_base story) + (obj - 1) * object_table_entry_size + 4) new_parent;;
    
    let object_sibling story obj = 
        read_ubyte story ((object_tree_base story) + (obj - 1) * object_table_entry_size + 5);;

    let set_object_sibling story obj new_sibling = 
        write_byte story ((object_tree_base story) + (obj - 1) * object_table_entry_size + 5) new_sibling;;
        
    let object_child story obj = 
        read_ubyte story ((object_tree_base story) + (obj - 1) * object_table_entry_size + 6);;
        
    let set_object_child story obj new_child = 
        write_byte story ((object_tree_base story) + (obj - 1) * object_table_entry_size + 6) new_child;;
        
    let object_property_address story n = 
        read_word story ((object_tree_base story) + (n - 1) * object_table_entry_size + 7);;
       
    let object_count story =
        ((object_property_address story 1) - (object_tree_base story)) / object_table_entry_size;;
        
    let object_name story n = 
        let length = read_ubyte story (object_property_address story n) in
        if length = 0 then "<unnamed>" 
        else read_zstring story ((object_property_address story n) + 1);;
        
    let find_previous_sibling story child =
        let rec aux current =
            let next_sibling = object_sibling story current in
            if next_sibling = child then current
            else aux next_sibling in
        let parent = object_parent story child in
        let first_child = object_child story parent in
        aux first_child;;
        
    let insert_object story child parent =
        let original_parent = object_parent story child in
        let edit1 = 
            if original_parent <> 0 then (
                let first_child_of_original_parent = object_child story original_parent in
                if child = first_child_of_original_parent then
                    let new_first_child = object_sibling story child in
                    set_object_child story original_parent new_first_child
                else
                    let previous_sibling = find_previous_sibling story child in
                    let next_sibling = object_sibling story child in
                    set_object_sibling story previous_sibling next_sibling)
            else story in
        let edit2 = set_object_parent edit1 child parent in
        let old_first_child = object_child edit2 parent in
        let edit3 = set_object_sibling edit2 child old_first_child in
        set_object_child edit3 parent child;;
        
    (* Produces a list of (number, length, address) tuples *)
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
        
    let property_length_from_address story address =
        if address = 0 then 0 
        else
            let b = read_ubyte story (address - 1) in
            1 + fetch_bits 7 3 b;;
        
    let property_address story object_number property_number =
        let rec aux addresses =
            match addresses with
            | [] -> 0
            | (number, _, address) :: tail -> if number = property_number then address else aux tail in
        aux (property_addresses story object_number);;
        
    let object_property story object_number property_number =
        let rec aux addresses =
            match addresses with
            | [] -> default_property_value story property_number
            | (number, length, address) :: tail -> 
                if number = property_number then (
                    if length = 1 then
                        read_ubyte story address 
                    else if length = 2 then
                        read_word story address
                    else
                        failwith "bad property length")
                else
                    aux tail in
        aux (property_addresses story object_number);;
        
    let write_property story object_number property_number value =
        let rec aux addresses =
            match addresses with
            | [] -> (0, 0)
            | (number, length, address) :: tail -> if number = property_number then (address, length) else aux tail in
        let (address, length) = aux (property_addresses story object_number) in
        if address = 0 then failwith "invalid property";
        match length with
        | 1 -> write_byte story address value
        | 2 -> write_word story address value
        | _ -> failwith "property cannot be set";;
            
    let display_properties story object_number =
        List.fold_left (fun s (property_number, length, address) -> 
            s ^ 
            (Printf.sprintf "%02x" property_number) ^ 
            (if length = 1 || length = 2 then Printf.sprintf ":%04x " (object_property story object_number property_number) else " "))
            "" 
            (property_addresses story object_number);; 
       
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
        
    (* TODO: 9 in v4 and up *)
    let dictionary_max_word_length = 6;;
        
    let dictionary_entry_count story =     
        read_word story ((dictionary_base story) + (word_separators_count story) + 2);;
    
    let dictionary_table_base story =
        (dictionary_base story) + (word_separators_count story) + 4;;
        
    let dictionary_entry_address story dictionary_number =
        (dictionary_table_base story) + dictionary_number * (dictionary_entry_length story);;
        
    let dictionary_entry story dictionary_number =
        read_zstring story (dictionary_entry_address story dictionary_number);;
        
    (* does text1 start with text2? *)
    let starts_with text1 text2 =
        let len1 = String.length text1 in
        let len2 = String.length text2 in
        let rec aux i =
            if i = len2 then true
            else if i = len1 then false 
            else if text1.[i] <> text2.[i] then false
            else aux (i + 1) in
        aux 0;;
        
    let dictionary_lookup story text =
        (* TODO: Could make this more efficient via binary search *)
        let count = dictionary_entry_count story in
        let truncated = if (String.length text) > dictionary_max_word_length then String.sub text 0 dictionary_max_word_length else text in
        let rec aux i =
            if i = count then 0
            else if truncated = dictionary_entry story i then dictionary_entry_address story i
            else aux (i + 1) in
        aux 0;;
    
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
        | OP2_15  -> "loadw"
        | OP2_16  -> "loadb"
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
        
        type branch_address = 
        | Return_true  
        | Return_false
        | Branch_address of int
        
        type instruction =
        {
            opcode : bytecode;
            address : int;
            length : int;
            operands : operand list;
            store : variable_location option;
            branch : (bool * branch_address) option;
            text : string option;
        };;
    
        (* TODO: Only works for version 3 *)
        let resolve_packed_address addr = 
            addr * 2;;
    
        let is_call opcode = 
            match opcode with
            | VAR_224 (* call / call_vs *)
            | OP1_143 (* call_1n *)
            | OP1_136 (* call_1s *)
            | OP2_26 (* call_2n *)
            | OP2_25 (* call_2s *)
            | VAR_249 (* call_vn *)
            | VAR_250 (* call_vn2 *)
            | VAR_236 (* call_vs2 *) -> true
            | _ -> false;;
    
    
    (* Takes the address of an instruction and produces the instruction *)
    
    let decode_instruction story address =
        
        (* Helper methods for decoding *)
         
        let has_branch opcode = 
            match opcode with
            | OP2_1   | OP2_2   | OP2_3   | OP2_4   | OP2_5   | OP2_6   | OP2_7   | OP2_10 
            | OP1_128 | OP1_129 | OP1_130 | OP0_181 | OP0_182 | OP0_189 | OP0_191
            | VAR_247 | VAR_255 -> true
            | _ -> false in
    
        let has_store opcode =
            match opcode with
            | OP2_8   | OP2_9   | OP2_15  | OP2_16  | OP2_17  | OP2_18  | OP2_19
            | OP2_20  | OP2_21  | OP2_22  | OP2_23  | OP2_24  | OP2_25
            | OP1_129 | OP1_130 | OP1_131 | OP1_132 | OP1_136 | OP1_142 | OP1_143
            | VAR_224 | VAR_231 | VAR_236 | VAR_246 | VAR_247 | VAR_248 -> true
            | _ -> false in
            
        let has_text opcode =
            match opcode with
            | OP0_178 | OP0_179 -> true
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
        
        let branch_size branch_code_address =
            let b = (read_ubyte story branch_code_address) in 
            if (fetch_bit 6 b) then 1 else 2 in
        
        let decode_branch branch_code_address total_length =
            let b = (read_ubyte story branch_code_address) in 
            let sense = (fetch_bit 7 b) in
            let bottom6 = fetch_bits 5 6 b in
            let offset = 
                if (fetch_bit 6 b) then 
                    bottom6 
                else
                    let unsigned = 256 * bottom6 + (read_ubyte story (branch_code_address + 1)) in
                    if unsigned < 8192 then unsigned else unsigned - 16384 in
            match offset with
            | 0 -> (sense, Return_false)
            | 1 -> (sense, Return_true)
            | _ -> (sense, Branch_address (address + total_length + offset - 2)) in
                
        let munge_operands instr =
            let munge_store_operands () =
                (* The first operand must be a variable, but it is sometimes a value instead *)
                match instr.operands with
                | (Small small) :: tail -> (Variable (decode_variable small)) :: tail 
                | _ -> instr.operands in   
            let munge_call_operands () =
                match instr.operands with
                | (Large large) :: tail -> (Large (resolve_packed_address large)) :: tail
                | _ -> instr.operands in
            let munge_jump_operands () =
                match instr.operands with
                | [(Large large)] -> [(Large (instr.address + instr.length + (signed_word large) - 2))]
                | _ -> failwith "jump requires one large operand" in 
                (* is this true? Can jump offset be taken from stack or variable? *)
                (* Can a jump be small? If so, is it signed? *)
            match instr.opcode with
            | OP2_4   (* dec_chk *)
            | OP2_5   (* inc_chk *) 
            | OP2_13  (* store *)
            | OP1_133 (* inc *)
            | OP1_134 (* dec *)
            | OP1_142 (* load *) 
            | VAR_233 (* pull *) -> munge_store_operands()
            | OP1_140 -> munge_jump_operands()
            | _ -> if (is_call instr.opcode) then munge_call_operands() else instr.operands in
            
        (* Helper methods are done. Start decoding *)
        
        (*  SPEC
        
        4.3 Form and operand count
        
        Each instruction has a form (long, short, extended or variable) and an operand count (0OP, 1OP,
        2OP or VAR). If the top two bits of the opcode are $$11 the form is variable; if $$10, the form is
        short. If the opcode is 190 ($BE in hexadecimal) and the version is 5 or later, the form is "extended".
        Otherwise, the form is "long". 
        
        4.3.1
        
        In short form, bits 4 and 5 of the opcode byte give an operand type as above. If this is $11 then
        the operand count is 0OP; otherwise, 1OP. In either case the opcode number is given in the bottom
        4 bits.
        
        4.3.2
        
        In long form the operand count is always 2OP. The opcode number is given in the bottom 5 bits.
        
        4.3.3
        
        In variable form, if bit 5 is 0 then the count is 2OP; if it is 1, then the count is VAR. The opcode
        number is given in the bottom 5 bits.
        
        4.3.4 (TODO)
        
        In extended form, the operand count is VAR. The opcode number is given in a second opcode
        byte.
        
        *)
    
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
        
        (* SPEC 
         
        4.4 Specifying operand types
        
        Next, the types of the operands are specified.
        
        4.4.1
        
        In short form, bits 4 and 5 of the opcode give the type.
        
        4.4.2
        
        In long form, bit 6 of the opcode gives the type of the first operand, bit 5 of the second. A value
        of 0 means a small constant and 1 means a variable. (If a 2OP instruction needs a large constant
        as operand, then it should be assembled in variable rather than long form.)
        
        4.4.3
        
        In variable or extended forms, a byte of 4 operand types is given next. This contains 4 2-bit
        fields: bits 6 and 7 are the first field, bits 0 and 1 the fourth. The values are operand types as
        above. Once one type has been given as 'omitted', all subsequent ones must be. Example:
        $$00101111 means large constant followed by variable (and no third or fourth opcode).`
       
        *)
        
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
        let store = if has_store opcode then Some (decode_variable (read_ubyte story store_address)) else None in
        let store_length = if has_store opcode then 1 else 0 in
        let branch_code_address = store_address + store_length in
        let branch_length = if has_branch opcode then (branch_size branch_code_address) else 0 in
        let text_address = branch_code_address + branch_length in
        let text = if has_text opcode then Some (read_zstring story text_address) else None in
        let text_length = if has_text opcode then (zstring_length story text_address) else 0 in
        let total_length = opcode_length + type_length + operand_length + store_length + branch_length + text_length in
        let branch = if has_branch opcode then Some (decode_branch branch_code_address total_length) else None in
        let instr = {opcode; address; length = total_length; operands; store; branch; text} in
        { instr with operands = munge_operands instr };;

    let display_instruction instr =
        let display_variable variable =
            match variable with
            | Stack -> "stack"
            | Local local -> Printf.sprintf "local%01x" local 
            | Global global -> Printf.sprintf "global%02x" global in
    
        let display_operands () = 
            let display_operand operand =
                match operand with
                | Large large -> Printf.sprintf "%04x" large
                | Small small -> Printf.sprintf "%02x" small
                | Variable variable -> (display_variable variable) in
            List.fold_left (fun acc operand -> acc ^ (display_operand operand) ^ " ") "" instr.operands in
            
        let display_store () =
            match instr.store with
            | None -> ""
            | Some variable -> "->" ^ (display_variable variable) in   
            
        let display_branch () = 
            match instr.branch with
            | None -> ""
            | Some (sense, Return_false) -> Printf.sprintf "if %B return false" sense 
            | Some (sense, Return_true) -> Printf.sprintf "if %B return true" sense 
            | Some (sense, Branch_address address) -> Printf.sprintf "if %B goto %04x" sense address in
           
        let display_text () =
            match instr.text with
            | None -> ""
            | Some str -> str in   
           
        let start_addr = instr.address in
        let name = opcode_name instr.opcode in
        let operands = display_operands () in
        let store = display_store() in 
        let branch = display_branch() in
        let text = display_text() in
        Printf.sprintf "%04x: %s %s%s %s %s\n" start_addr name operands store branch text;;
        
    let display_instructions story address count =
        let rec aux acc addr c =
            if c = 0 then acc
            else (
                let instr = decode_instruction story addr in
                let s = display_instruction instr in
                aux (acc  ^ s) (addr + instr.length) (c - 1)) in
        aux "" address count;;
        
    let continues_to_following opcode =
        match opcode with
        | OP2_28 (* throw *)
        | OP1_139 (* ret *)
        | OP1_140 (* jump *)
        | OP0_176 (* rtrue *)
        | OP0_177 (* rfalse *)
        | OP0_179 (* print_ret *)
        | OP0_183 (* restart *)
        | OP0_184 (* ret_popped *) 
        | OP0_186 (* quit *) -> false
        | _ -> true;;
        
    let branch_target instr =
        let br_target = 
            match instr.branch with
            | None -> None
            | Some (_, Return_false) -> None
            | Some (_, Return_true) -> None
            | Some (_, Branch_address address) -> Some address in
        let jump_target =
            match (instr.opcode, instr.operands) with
                | (OP1_140, [Large address]) -> Some address
                | _ -> None in
        match (br_target, jump_target) with
        | (Some b, _) -> Some b
        | (_, Some j) -> Some j
        | _ -> None;;
        
    let all_reachable_addresses_in_routine story instr_address = 
        let immediately_reachable_addresses address = 
            let instr = decode_instruction story address in
            let next = if (continues_to_following instr.opcode) then [instr.address + instr.length] else [] in
            match (branch_target instr) with 
                | Some address -> address :: next
                | _ -> next in
        reflexive_closure instr_address immediately_reachable_addresses;;
        
    let display_reachable_instructions story address =
        let reachable = List.sort compare (all_reachable_addresses_in_routine story address) in
        List.fold_left (fun a b -> a ^ (display_instruction (decode_instruction story b))) "" reachable;;
 
    let locals_count story routine_address =
        let c = read_ubyte story routine_address in
        if c > 15 then failwith "routine must have fewer than 16 locals";
        c;;
 
    let first_instruction story routine_address =
        routine_address + 1 + (locals_count story routine_address) * 2 ;;
        
    (* Note that here the locals are indexed from 1 to 15, not 0 to 14 *)
    let local_default_value story routine_address n =
        if n < 1 || n > 15 then failwith "invalid local";
        read_word story (routine_address + 1 + 2 * (n - 1));;
     
    let display_routine story routine_address =
        display_reachable_instructions story (first_instruction story routine_address) ;;

    let call_address instr = 
        if (is_call instr.opcode) then 
            match instr.operands with 
            | (Large address) :: _ -> Some address
            | _ -> None 
        else None;;

    (* Takes the address of the first instruction in a routine, produces
       a list of addresses of all routines called in the routine. *)
       
    let reachable_routines_in_routine story instr_address =
        let reachable_instrs = all_reachable_addresses_in_routine story instr_address in
        let option_fold routines instr_addr = 
            match call_address (decode_instruction story instr_addr) with
            | None -> routines
            | Some routine_address -> routine_address :: routines in
        List.fold_left option_fold [] reachable_instrs;;
        
    let all_routines story =
        let ipc = initial_program_counter story in
        let called_by_main = reachable_routines_in_routine story ipc in
        let relation routine = 
            reachable_routines_in_routine story (first_instruction story routine) in      
        let all_routines = reflexive_closure_many called_by_main relation in
        List.sort compare all_routines;;
        
    (* TODO: Have this return a string? *)
    let display_all_routines story = 
        List.iter (fun r -> Printf.printf "\n---\n%s" (display_routine story r)) (all_routines story);;
    
    (* Note that globals are indexed starting at 16 *)
    let read_global story n = 
        if n < 16 || n > 255 then failwith "global variable index out of range";
        read_word story ((global_variables_table_base story) + (n - 16) * 2)
        
    let write_global story n value =
        if n < 16 || n > 255 then failwith "global variable index out of range";
        write_word story ((global_variables_table_base story) + (n - 16) * 2) value;;
        
end

module Interpreter = struct

    open Story;;
    
    type state = 
        | Running
        | Halted;;

    type frame =
    {
        stack : int list;
        locals : int IntMap.t;
        called_from : int
    };;

    type t = 
    {
        story : Story.t;
        program_counter : int;
        frames : frame list;
        random_w : Int32.t;
        random_x : Int32.t;
        random_y : Int32.t;
        random_z : Int32.t;
        state : state
    };;
    
    let make story = 
    { 
        story = story; 
        program_counter = Story.initial_program_counter story;
        frames = [ { stack = []; locals = IntMap.empty; called_from = 0 } ];
        (* TODO: Seed these randomly *)
        random_w = Int32.of_int 123;
        random_x = Int32.of_int 123;
        random_y = Int32.of_int 123;
        random_z = Int32.of_int 123;
        state = Running
    };;
    
    let random_next interp n =
        (* See wikipedia article on xorshift *)
        let t = Int32.logxor interp.random_x (Int32.shift_left interp.random_x 11) in
        let new_x = interp.random_y in
        let new_y = interp.random_z in
        let new_z = interp.random_w in
        let new_w = Int32.logxor (Int32.logxor (Int32.logxor interp.random_w (Int32.shift_right_logical interp.random_w 19)) t) (Int32.shift_right_logical t 8) in
        let result = 1 + (Int32.to_int (Int32.rem new_w (Int32.of_int n)) + n) mod n in
        (result, { interp with random_w = new_w; random_x = new_x; random_y = new_y; random_z = new_z });;
    
    let current_frame interpreter = 
        List.hd interpreter.frames;;
    
    let peek_stack interpreter = 
        List.hd (current_frame interpreter).stack;;
        
    let pop_stack interpreter =
        match interpreter.frames with
        | h :: t -> { interpreter with frames = ({ h with stack = List.tl h.stack }) :: t }
        | _ -> failwith "frame set is empty"
        
    let push_stack interpreter value =
        match interpreter.frames with
        | h :: t -> { interpreter with frames = ({ h with stack = value :: h.stack }) :: t }
        | _ -> failwith "frame set is empty"
   
    (* Reading operands can change the state of the interpreter, because it can
       pop the stack. *)

    let read_operand_no_pop interpreter operand =
        match operand with
        | Large large -> large
        | Small small -> small
        | Variable Stack -> peek_stack interpreter
        | Variable Local local -> IntMap.find local (current_frame interpreter).locals
        | Variable Global global -> Story.read_global interpreter.story global;;
        
    let read_operand interpreter operand =
        let value = read_operand_no_pop interpreter operand in
        match operand with
        | Variable Stack -> (value, pop_stack interpreter)
        | _ -> (value, interpreter);;
        
    (* TODO: Be smarter about packed addresses *)
    let read_address_operand interpreter operand =
        match operand with
        | Large large -> (large, interpreter)
        | Small small -> (small, interpreter)
        | Variable Stack -> (2 * (peek_stack interpreter), pop_stack interpreter)
        | Variable Local local -> (2 * (IntMap.find local (current_frame interpreter).locals), interpreter)
        | Variable Global global -> (2 * (Story.read_global interpreter.story global), interpreter);;
    
    let next_instruction interpreter instruction =
        { interpreter with program_counter = interpreter.program_counter + instruction.length };;
        
    let write_local interpreter local value =
        match interpreter.frames with
        | h :: t -> { interpreter with frames = ({ h with locals = IntMap.add local value h.locals }) :: t }
        | _ -> failwith "frame set is empty"
        
    let write_global interpreter global value = 
        { interpreter with story = Story.write_global interpreter.story global value };;

    let do_store interpreter variable value =
        match variable with
        | Local local -> write_local interpreter local value
        | Global global -> write_global interpreter global value
        | Stack -> push_stack interpreter value;;
 
    (* TODO: Is there a way to make this more elegant? *)

    let rec handle_branch interpreter instruction result =
        match instruction.branch with
        | None -> next_instruction interpreter instruction
        | Some (sense, Return_false) -> 
            if (result <> 0) = sense then handle_return interpreter instruction 0
            else next_instruction interpreter instruction
        | Some (sense, Return_true) -> 
            if (result <> 0) = sense then handle_return interpreter instruction 1
            else next_instruction interpreter instruction
        | Some (sense, Branch_address branch_target) -> 
            if (result <> 0) = sense then { interpreter with program_counter = branch_target }  
            else next_instruction interpreter instruction
            
    and handle_store_and_branch interpreter instruction result = 
        let store_interpreter = 
            match instruction.store with
            | None -> interpreter
            | Some variable -> do_store interpreter variable result in
        handle_branch store_interpreter instruction result
        
    and handle_return interpreter instruction value =
        let next_program_counter = (current_frame interpreter).called_from in
        let result_interpreter = { interpreter with program_counter = next_program_counter; frames = List.tl interpreter.frames } in
        let call_instr = Story.decode_instruction result_interpreter.story next_program_counter in
        handle_store_and_branch result_interpreter call_instr value;;
       
    let handle_op1 interpreter instruction compute_result = 
        match instruction.operands with
        | [x_operand] ->  
            let (x, operand_interpreter) = read_operand interpreter x_operand in
            let (result, result_interpreter) = compute_result x operand_interpreter in
            handle_store_and_branch result_interpreter instruction result
       | _ -> failwith "instruction must have one operand";;
    
    let handle_op2 interpreter instruction compute_result = 
        match instruction.operands with
        | [x_operand; y_operand] ->  
            let (x, x_interpreter) = read_operand interpreter x_operand in
            let (y, y_interpreter) = read_operand x_interpreter y_operand in
            let (result, result_interpreter) = compute_result x y y_interpreter in
            handle_store_and_branch result_interpreter instruction result
       | _ -> failwith (Printf.sprintf "instruction at %04x must have two operands" instruction.address );;
    
    let handle_op3 interpreter instruction compute_result = 
        match instruction.operands with
        | [x_operand; y_operand; z_operand] ->  
            let (x, x_interpreter) = read_operand interpreter x_operand in
            let (y, y_interpreter) = read_operand x_interpreter y_operand in
            let (z, z_interpreter) = read_operand y_interpreter z_operand in
            let (result, result_interpreter) = compute_result x y z z_interpreter in
            handle_store_and_branch result_interpreter instruction result
       | _ -> failwith (Printf.sprintf "instruction at %04x must have three operands" instruction.address );;
       
    let handle_op4 interpreter instruction compute_result = 
        match instruction.operands with
        | [w_operand; x_operand; y_operand; z_operand] ->  
            let (w, w_interpreter) = read_operand interpreter w_operand in
            let (x, x_interpreter) = read_operand w_interpreter x_operand in
            let (y, y_interpreter) = read_operand x_interpreter y_operand in
            let (z, z_interpreter) = read_operand y_interpreter z_operand in
            let (result, result_interpreter) = compute_result w x y z z_interpreter in
            handle_store_and_branch result_interpreter instruction result
       | _ -> failwith (Printf.sprintf "instruction at %04x must have four operands" instruction.address );;
    
    (* Handle calls -- TODO some of these can be made into local methods *)
        
    let create_default_locals story routine_address =
        let count = Story.locals_count story routine_address in
        let rec aux map i = 
            if i > count then map
            else aux (IntMap.add i (Story.local_default_value story routine_address i) map) (i + 1) in
        aux IntMap.empty 1;;
        
    (* 
        Always evaluate the operand -- we might be popping the stack
        If the local number is valid then update the locals map with
        the argument. *)
    
    let copy_argument_to_locals interpreter operand locals locals_count n =
        let (value, new_interpreter) = read_operand interpreter operand in
        let new_locals = if n <= locals_count then IntMap.add n value locals else locals in
        (new_locals, new_interpreter);;
        
    let copy_arguments_to_locals interpreter operands locals locals_count =
        let rec aux interpreter operands locals n =
            match operands with
            | [] -> (locals, interpreter)
            | operand :: tail -> 
                let (new_locals, new_interpreter) = copy_argument_to_locals interpreter operand locals locals_count n in
                aux new_interpreter tail new_locals (n + 1) in
            aux interpreter operands locals 1;;
       
    let handle_call interpreter instruction =
        let routine_address_operand = List.hd instruction.operands in
        let routine_operands = List.tl instruction.operands in
        let (routine_address, routine_interpreter) = read_address_operand interpreter routine_address_operand in
        let locals_count = Story.locals_count routine_interpreter.story routine_address in
        let default_locals = create_default_locals routine_interpreter.story routine_address in
        let (locals, locals_interpreter) = copy_arguments_to_locals routine_interpreter routine_operands default_locals locals_count in
        
        (* We have evaluated all the operands; at this point we need to bail if the 
           target address is zero *)
           
        if routine_address = 0 then 
            handle_store_and_branch locals_interpreter instruction 0
        else 
            let frame = { stack = []; locals = locals; called_from = instruction.address } in 
            let first_instruction = Story.first_instruction locals_interpreter.story routine_address in
            { locals_interpreter with story = locals_interpreter.story; program_counter = first_instruction; frames = frame :: interpreter.frames };;
        
    let handle_ret interpreter instruction = 
        match instruction.operands with
        | [lone_operand] ->  
            let (result, operand_interpreter) = read_operand interpreter lone_operand in
            handle_return operand_interpreter instruction result
        | _ -> failwith "instruction must have one operand";;
        
    let handle_ret_popped interpreter instruction = 
        let result = peek_stack interpreter in
        let popped_interpreter = pop_stack interpreter in
        handle_return popped_interpreter instruction result;;
        
    let handle_jump interpreter instruction =
        match instruction.operands with
        | [target_operand] ->  
            let (target, target_interpreter) = read_operand interpreter target_operand in
            { target_interpreter with program_counter = target }
        | _ -> failwith "instruction must have one operand";;
       
    (* TODO: These instructions treat variables as storage rather than values *)
    (* TODO: There may be a way to consolidate the code here *)
        
    let do_store_in_place interpreter variable value = 
        match variable with
        | Local local -> write_local interpreter local value
        | Global global -> write_global interpreter global value
        | Stack -> push_stack (pop_stack interpreter) value;;
        
    let handle_store interpreter instruction =
        match instruction.operands with
        | [(Variable variable); value_operand] ->  
            let (value, value_interpreter) = read_operand interpreter value_operand in 
            let store_interpreter = do_store_in_place value_interpreter variable value in
            handle_branch store_interpreter instruction 0
        | _ -> failwith "store requires a variable and a value";;
        
    let handle_inc_chk interpreter instruction = 
        match instruction.operands with
        | [(Variable variable) as variable_operand ; test_operand] -> 
            let original = read_operand_no_pop interpreter variable_operand in 
            let incremented = signed_word (original + 1) in
            let store_interpreter = do_store_in_place interpreter variable incremented in
            let (test, test_interpreter) = read_operand store_interpreter test_operand in
            let result = if (signed_word incremented) > (signed_word test) then 1 else 0 in
            handle_branch test_interpreter instruction result
        | _ -> failwith "inc_chk requires a variable and a value";;
        
    let handle_dec_chk interpreter instruction = 
        match instruction.operands with
        | [(Variable variable) as variable_operand ; test_operand] -> 
            let original = read_operand_no_pop interpreter variable_operand in 
            let incremented = signed_word (original - 1) in
            let store_interpreter = do_store_in_place interpreter variable incremented in
            let (test, test_interpreter) = read_operand store_interpreter test_operand in
            let result = if (signed_word incremented) < (signed_word test) then 1 else 0 in
            handle_branch test_interpreter instruction result
        | _ -> failwith "dec_chk requires a variable and a value";;
        
    let handle_inc interpreter instruction = 
        match instruction.operands with
        | [(Variable variable) as variable_operand] -> 
            let original = read_operand_no_pop interpreter variable_operand in 
            let incremented = signed_word (original + 1) in
            let store_interpreter = do_store_in_place interpreter variable incremented in
            handle_branch store_interpreter instruction 0
        | _ -> failwith "inc requires a variable";;
        
    let handle_dec interpreter instruction = 
        match instruction.operands with
        | [(Variable variable) as variable_operand] -> 
            let original = read_operand_no_pop interpreter variable_operand in 
            let incremented = signed_word (original - 1) in
            let store_interpreter = do_store_in_place interpreter variable incremented in
            handle_branch store_interpreter instruction 0
        | _ -> failwith "dec requires a variable";;
        
    let handle_pull interpreter instruction =
        match instruction.operands with
        | [(Variable variable)] ->  
            let value = peek_stack interpreter in
            let popped_interpreter = pop_stack interpreter in
            let store_interpreter = do_store_in_place popped_interpreter variable value in
            handle_branch store_interpreter instruction 0
        | _ -> failwith "pull requires a variable ";;
      
    (* TODO: Consolidate the code in the printing methods *)
       
    let interpreter_print text = 
        print_string text;
        flush stdout;;
       
    let handle_print interpreter instruction =
        (match instruction.text with
        | Some text -> interpreter_print text
        | _ -> failwith "no text in print instruction");
        handle_branch interpreter instruction 0;;
        
    let handle_print_ret interpreter instruction =
        (match instruction.text with
        | Some text -> interpreter_print text
        | _ -> failwith "no text in print_ret instruction");
        handle_return interpreter instruction 1;;
        
    let handle_new_line interpreter instruction = 
        interpreter_print "\n";
        handle_branch interpreter instruction 0;;
        
    let handle_quit interpreter instruction =
        { interpreter with state = Halted };;
    
    (* je is interesting in that it is a 2OP that can take 2 to 4 operands. *)
    let handle_je interpreter instruction = 
        let handle_je2 test x interp = ((if (signed_word test) = (signed_word x) then 1 else 0), interp) in
        let handle_je3 test x y interp = ((let test = (signed_word test) in if test = (signed_word x) || test == (signed_word y) then 1 else 0), interp) in
        let handle_je4 test x y z interp = ((let test = (signed_word test) in if test = (signed_word x) || test = (signed_word y) || test == (signed_word z) then 1 else 0), interp) in
        match instruction.operands with
        | [_; _] -> handle_op2 interpreter instruction handle_je2
        | [_; _; _] -> handle_op3 interpreter instruction handle_je3
        | [_; _; _; _] -> handle_op4 interpreter instruction handle_je4
        | _ -> failwith "je instruction requires 2 to 4 operands";;
        
    let handle_sread text_address parse_address interp =
    
        (* TODO: Get word separator list from story *)
    
        let tokenise text = 
            let length = String.length text in
            let rec find_space_or_end i = 
                if i = length then i
                else if text.[i] = ' ' then i
                else find_space_or_end (i + 1) in
            
            let rec skip_spaces i =
                if i = length then i 
                else if text.[i] = ' ' then skip_spaces (i + 1)
                else i in
                
            let rec token start =
                if start = length then 
                    None
                else 
                    let end_of_token = find_space_or_end start in
                    let token_text = String.sub text start (end_of_token - start) in
                    let dictionary_address = dictionary_lookup interp.story token_text in
                    Some (token_text, start, dictionary_address) in
                    
            let rec aux i acc = 
                match token i with
                | None -> acc
                | Some (tok, start, addr) -> aux (skip_spaces (i + String.length tok)) ((tok, start, addr) :: acc) in
            
            List.rev (aux (skip_spaces 0) []) in
            
            
            
        
        (* SPEC
        
        This opcode reads a whole command from the keyboard (no prompt is automatically displayed).
        
        It is legal for this to be called with the cursor at any position on any window.
        
        TODO: In Versions 1 to 3, the status line is automatically redisplayed first.
        
        A sequence of characters is read in from the current input stream until a carriage return (or, in
        Versions 5 and later, any terminating character) is found.
        
        In Versions 1 to 4, byte 0 of the text-buffer should initially contain the maximum number of
        letters which can be typed, minus 1 (the interpreter should not accept more than this).
        
        The text typed is reduced to lower case (so that it can tidily be printed back by the program if need be)
        and stored in bytes 1 onward, with a zero terminator (but without any other terminator, such as a
        carriage return code). (This means that if byte 0 contains n then the buffer must contain n+1
        bytes, which makes it a string array of length n in Inform terminology.)
        *)
        
        (* TODO: Should restrict input to this many chars, not trim it later *)
        
        let maximum_letters = read_ubyte interp.story text_address in
        
        (* 
        Interpreters are asked to halt with a suitable error message if the text or parse buffers have
        length of less than 3 or 6 bytes, respectively: this sometimes occurs due to a previous array being
        overrun, causing bugs which are very difficult to find.
        *)
        
        if maximum_letters < 3 then failwith "bad text buffer in sread";
        
        let text = String.lowercase (input_line stdin) in
        let trimmed = if (String.length text) > maximum_letters then String.sub text 0 maximum_letters else text in
        let string_copied_interpreter = { interp with story = write_string interp.story (text_address + 1) trimmed } in
        
        (*
        
        TODO: This section only relevant to V4 and greater
        
        In Versions 5 and later, byte 0 of the text-buffer should initially contain the maximum number
        of letters which can be typed (the interpreter should not accept more than this). The interpreter
        stores the number of characters actually typed in byte 1 (not counting the terminating character),
        and the characters themselves in bytes 2 onward (not storing the terminating character). (Some
        interpreters wrongly add a zero byte after the text anyway, so it is wise for the buffer to contain
        at least n+3 bytes.)
        
        Moreover, if byte 1 contains a positive value at the start of the input, then read assumes that
        number of characters are left over from an interrupted previous input, and writes the new characters
        after those already there. Note that the interpreter does not redisplay the characters left
        over: the game does this, if it wants to. This is unfortunate for any interpreter wanting to give input
        text a distinctive appearance on-screen, but 'Beyond Zork', 'Zork Zero' and 'Shogun' clearly
        require it. ("Just a tremendous pain in my butt" -- Andrew Plotkin; "the most unfortunate feature
        of the Z-machine design" -- Stefan Jokisch.)
        
        In Version 4 and later, if the operands time and routine are supplied (and non-zero) then the
        routine call routine() is made every time/10 seconds during the keyboard-reading process. If this
        routine returns true, all input is erased (to zero) and the reading process is terminated at once.
        (The terminating character code is 0.) The routine is permitted to print to the screen even if it
        returns false to signal "carry on": the interpreter should notice and redraw the input line so far,
        before input continues. (Frotz notices by looking to see if the cursor position is at the left-hand
        margin after the interrupt routine has returned.)
        
        *)
        
        (*
        If input was terminated in the usual way, by the player typing a carriage return, then a carriage
        return is printed (so the cursor moves to the next line). If it was interrupted, the cursor is left at
        the rightmost end of the text typed in so far.*)
        
        interpreter_print "\n";
        
        (* 
        Next, lexical analysis is performed on the text (except that in Versions 5 and later, if parsebuffer
        is zero then this is omitted). Initially, byte 0 of the parse-buffer should hold the maximum
        number of textual words which can be parsed. (If this is n, the buffer must be at least 2 +
        4*n bytes long to hold the results of the analysis.) 
        *)
        
        let maximum_parse = read_ubyte string_copied_interpreter.story parse_address in
        
        if maximum_parse < 1 then failwith "bad parse buffer in sread";
        
        (*        
        
        The interpreter divides the text into words and looks them up in the dictionary
        
        The number of words is written in byte 1 and one 4-byte block is written for each word, from
        byte 2 onwards (except that it should stop before going beyond the maximum number of words
        specified). 
        
        Each block consists of the byte address of the word in the dictionary, if it is in the
        dictionary, or 0 if it isn't; followed by a byte giving the number of letters in the word; and finally
        a byte giving the position in the text-buffer of the first letter of the word.
        
        In Version 5 and later, this is a store instruction: the return value is the terminating character
        (note that the user pressing his "enter" key may cause either 10 or 13 to be returned; the author
        recommends that interpreters return 10). 
        
        A timed-out input returns 0.
        
        Versions 1 and 2 and early Version 3 games mistakenly write the parse buffer length 240 into
        byte 0 of the parse buffer: later games fix this bug and write 59, because 2+4*59 = 238 so that 59
        is the maximum number of textual words which can be parsed into a buffer of length 240 bytes.
        Old versions of the Inform 5 library commit the same error. Neither mistake has very serious
        consequences.
        
        *)
        
        let tokens = tokenise trimmed in
        
        let rec write_tokens items address count writing_tokens_interpreter =
            match items with
            | [] -> (count, writing_tokens_interpreter)
            | (tok, text_offset, dictionary_address) :: tail -> 
                if count = maximum_parse then 
                    (count, writing_tokens_interpreter)
                else 
               ( 
                    let addr_story = write_word writing_tokens_interpreter.story address dictionary_address in
                    let len_story = write_byte addr_story (address + 2) (String.length tok) in
                    let offset_story = write_byte len_story (address + 3) (text_offset + 1) in
                    write_tokens tail (address + 4) (count + 1) { writing_tokens_interpreter with story = offset_story } ) in
                    
        let (count, tokens_written_interpreter) =  write_tokens tokens (parse_address + 2) 0 string_copied_interpreter in
        
        (* TODO: Make a write byte that takes interpreters *)
       
        let length_copied_interpreter = { tokens_written_interpreter with story = write_byte tokens_written_interpreter.story (parse_address + 1) count } in
        
        (0, length_copied_interpreter) ;;
        
    let step interpreter =
        let handle_jl x y interp = ((if (signed_word x) < (signed_word y) then 1 else 0), interp) in
        let handle_jg x y interp = ((if (signed_word x) > (signed_word y) then 1 else 0), interp) in
        let handle_jin x y interp = ((if (object_parent interp.story x) = y then 1 else 0), interp) in
        let handle_test x y interp = ((if ((unsigned_word x) land (unsigned_word y)) = (unsigned_word y) then 1 else 0), interp) in
        let handle_or x y interp = (((unsigned_word x) lor (unsigned_word y)), interp) in
        let handle_and x y interp = (((unsigned_word x) land (unsigned_word y)), interp) in
        let handle_test_attr obj attr interp = ((if (Story.object_attribute interp.story obj attr) then 1 else 0), interp) in
        let handle_set_attr obj attr interp = (0, { interp with story = set_object_attribute interp.story obj attr } ) in
        let handle_clear_attr obj attr interp = (0, { interp with story = clear_object_attribute interp.story obj attr } ) in
        let handle_insert_obj child parent interp = (0, { interp with story = insert_object interp.story child parent } ) in
        let handle_loadw arr ind interp = (Story.read_word interp.story (arr + ind * 2), interp) in
        let handle_loadb arr ind interp = (Story.read_ubyte interp.story (arr + ind), interp) in
        let handle_get_prop obj prop interp = (object_property interp.story obj prop, interp) in
        let handle_get_prop_addr obj prop interp = (Story.property_address interp.story obj prop, interp) in
        let handle_add x y interp = ((signed_word (x + y)), interp) in
        let handle_sub x y interp = ((signed_word (x - y)), interp) in
        let handle_mul x y interp = ((signed_word (x * y)), interp) in
        let handle_div x y interp = ((signed_word (x / y)), interp) in
        let handle_mod x y interp = ((signed_word (x mod y)), interp) in
        let handle_jz x interp = ((if x = 0 then 1 else 0), interp) in
        let handle_get_sibling x interp = (object_sibling interp.story x, interp) in
        let handle_get_child x interp = (object_child interp.story x, interp) in
        let handle_get_parent x interp = (object_parent interp.story x, interp) in
        let handle_get_prop_len x interp = (property_length_from_address interp.story x, interp) in
        let handle_print_obj x interp = (interpreter_print (object_name interp.story x); 0, interp) in
        (* TODO: Better job of handling packed addresses *)
        let handle_print_paddr x interp = (interpreter_print (read_zstring interp.story (x * 2)); 0, interp) in
        let handle_rtrue interp instr = handle_return interp instr 1 in
        let handle_rfalse interp instr = handle_return interp instr 0 in
        let handle_storew arr ind value interp = (0, { interp with story = write_word interp.story (arr + ind * 2) value }) in
        let handle_storeb arr ind value interp = (0, { interp with story = write_byte interp.story (arr + ind) value }) in
        let handle_putprop obj prop value interp = (0, { interp with story = write_property interp.story obj prop value }) in
        let handle_print_char x interp = (interpreter_print (Printf.sprintf "%c" (char_of_int x)); 0, interp) in
        let handle_print_num x interp = (interpreter_print (Printf.sprintf "%d" x); 0, interp) in
        let handle_push x interp = (0, push_stack interp x) in
        let handle_random x interp = 
            if x = 0 then 
                (0, { interp with random_w = (Random.self_init(); Random.int32 (Int32.of_int 1000000)) })
            else if x < 0 then
                (0, { interp with random_w = Int32.of_int x; random_x = Int32.of_int 123; random_y = Int32.of_int 123; random_z = Int32.of_int 123 })
            else
                random_next interp x in
    
        let instruction = Story.decode_instruction interpreter.story interpreter.program_counter in
        match instruction.opcode with
        | OP2_1   -> handle_je interpreter instruction 
        | OP2_2   -> handle_op2 interpreter instruction handle_jl
        | OP2_3   -> handle_op2 interpreter instruction handle_jg
        | OP2_4   -> handle_dec_chk interpreter instruction
        | OP2_5   -> handle_inc_chk interpreter instruction
        | OP2_6   -> handle_op2 interpreter instruction handle_jin
        | OP2_7   -> handle_op2 interpreter instruction handle_test
        | OP2_8   -> handle_op2 interpreter instruction handle_or
        | OP2_9   -> handle_op2 interpreter instruction handle_and
        | OP2_10  -> handle_op2 interpreter instruction handle_test_attr
        | OP2_11  -> handle_op2 interpreter instruction handle_set_attr
        | OP2_12  -> handle_op2 interpreter instruction handle_clear_attr
        | OP2_13  -> handle_store interpreter instruction 
        | OP2_14  -> handle_op2 interpreter instruction handle_insert_obj
        | OP2_15  -> handle_op2 interpreter instruction handle_loadw
        | OP2_16  -> handle_op2 interpreter instruction handle_loadb
        | OP2_17  -> handle_op2 interpreter instruction handle_get_prop
        | OP2_18  -> handle_op2 interpreter instruction handle_get_prop_addr
        
        | OP2_20  -> handle_op2 interpreter instruction handle_add
        | OP2_21  -> handle_op2 interpreter instruction handle_sub 
        | OP2_22  -> handle_op2 interpreter instruction handle_mul
        | OP2_23  -> handle_op2 interpreter instruction handle_div
        | OP2_24  -> handle_op2 interpreter instruction handle_mod
        
        | OP1_128 -> handle_op1 interpreter instruction handle_jz
        | OP1_129 -> handle_op1 interpreter instruction handle_get_sibling
        | OP1_130 -> handle_op1 interpreter instruction handle_get_child
        | OP1_131 -> handle_op1 interpreter instruction handle_get_parent
        | OP1_132 -> handle_op1 interpreter instruction handle_get_prop_len
        | OP1_133 -> handle_inc interpreter instruction 
        | OP1_134 -> handle_dec interpreter instruction 
        
        | OP1_138 -> handle_op1 interpreter instruction handle_print_obj
        | OP1_139 -> handle_ret interpreter instruction 
        | OP1_140 -> handle_jump interpreter instruction 
        | OP1_141 -> handle_op1 interpreter instruction handle_print_paddr
        
        | OP0_176 -> handle_rtrue interpreter instruction
        | OP0_177 -> handle_rfalse interpreter instruction
        | OP0_178 -> handle_print interpreter instruction
        | OP0_179 -> handle_print_ret interpreter instruction
        
        | OP0_184 -> handle_ret_popped interpreter instruction
        
        | OP0_186 -> handle_quit interpreter instruction
        | OP0_187 -> handle_new_line interpreter instruction
        
        | VAR_224 -> handle_call interpreter instruction
        | VAR_225 -> handle_op3 interpreter instruction handle_storew
        | VAR_226 -> handle_op3 interpreter instruction handle_storeb
        | VAR_227 -> handle_op3 interpreter instruction handle_putprop
        | VAR_228 -> handle_op2 interpreter instruction handle_sread
        | VAR_229 -> handle_op1 interpreter instruction handle_print_char
        | VAR_230 -> handle_op1 interpreter instruction handle_print_num
        | VAR_231 -> handle_op1 interpreter instruction handle_random 
        | VAR_232 -> handle_op1 interpreter instruction handle_push
        | VAR_233 -> handle_pull interpreter instruction 
        
        | _ -> failwith (Printf.sprintf "instruction not yet implemented:%s" (Story.display_instruction instruction));;
        
    let display_locals interpreter = 
        IntMap.fold (fun local value acc -> acc ^ (Printf.sprintf "local%01x=%04x " local value)) (current_frame interpreter).locals "";;
        
    let display_stack interpreter = 
        List.fold_left (fun str x -> Printf.sprintf "%s %04x" str x ) "" (current_frame interpreter).stack;;
        
    let display_interpreter interpreter = 
        let locals = display_locals interpreter in
        let stack = display_stack interpreter in
        let instr = Story.display_instructions interpreter.story interpreter.program_counter 1 in
        locals ^ "\n" ^ stack ^ "\n" ^ instr;;

    (* TODO: Will need to signal a halted interpreter somehow. *)
    let rec run interpreter =
(*         print_endline (display_interpreter interpreter);      *)
        match interpreter.state with
        | Halted -> ()
        | Running -> run (step interpreter);;

end

let story = Story.load_story "ZORK1.DAT";;

(*
print_endline (Story.display_header story);;
print_endline (Story.display_default_property_table story);;
print_endline (Story.display_object_tree story);;
print_endline (Story.display_object_table story);;
print_endline (Story.display_properties story 0xb4);;
print_endline (Story.display_bytes story (Story.object_property_address story 0xb4) 64);;
print_endline (Story.display_zchar_bytes story (1 + (Story.object_property_address story 0xb4)) 64);;
Story.display_all_routines story;; 
print_endline (Story.display_reachable_instructions story (Story.initial_program_counter story));; 
print_endline (display_abbreviation_table s);;
print_endline (display_default_property_table s);; 
print_endline (Story.display_dictionary story);;  
*)

let interp = Interpreter.make story;;
Interpreter.run interp;;

