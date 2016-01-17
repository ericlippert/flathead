(* Z-Machine tools written in OCaml, as part of my efforts to learn the language. *)

(* Some helper methods to start with. *)

(* Turns a given character into a string *)

let string_of_char x = String.make 1 x;;

(* Takes a string, a character and an index; finds
the highest index that matches the character at or before
the given index. If there is no match then None is returned. *)

let rec reverse_index_from text target index =
    if index < 0 then None
    else if text.[index] = target then Some index
    else reverse_index_from text target (index - 1);;

(* Takes a list and a count; produces a new list which has up to
   that many items taken from the head of the given list *)

let take items n =
    let rec aux items n acc =
    match (items, n) with
    | (_, 0) -> acc
    | ([], _) -> acc
    | ((h :: t), _) -> aux t (n - 1) (h :: acc) in
    List.rev (aux items n []);;

(* Takes a list and a count; removes the given number of items from
   the head of the list. *)

let rec drop items n =
    match(items, n) with
    | (_, 0) -> items
    | ([], _) -> []
    | (h :: t, _) -> drop t (n - 1);;

module Deque = struct

    (* Simplified version of Chris Okasaki's deque. *)

    let c = 3;;

    type 'a t =
    {
        front : 'a list;
        front_length : int;
        back : 'a list;
        back_length : int
    };;

    (* Invariants: front_length and back_length are the lengths of the lists *)
    (* Invariants: front_length <= c * back_length + 1 *)
    (* Invariants: back_length <= c * front_length + 1 *)

    exception Empty;;
    exception InvalidIndex;;

    let empty = { front = []; front_length = 0; back = []; back_length = 0 };;

    let is_empty deque = deque.front_length + deque.back_length = 0;;

    let balance deque =
        if deque.front_length > c * deque.back_length + 1 then
            let new_front_length = (deque.front_length + deque.back_length) / 2 in
            let new_back_length = deque.front_length + deque.back_length - new_front_length in
            {
                front = take deque.front new_front_length;
                front_length = new_front_length;
                back = deque.back @ (List.rev (drop deque.front new_front_length));
                back_length = new_back_length
            }
        else if deque.back_length > c * deque.front_length + 1 then
            let new_front_length = (deque.front_length + deque.back_length) / 2 in
            let new_back_length = deque.front_length + deque.back_length - new_front_length in
            {
                front = deque.front @ List.rev(drop deque.back new_back_length);
                front_length = new_front_length;
                back = take deque.back new_back_length;
                back_length = new_back_length
            }
        else deque;;

    let enqueue_front deque item =
        balance { deque with front = item :: deque.front; front_length = deque.front_length + 1};;

    let enqueue_back deque item =
        balance { deque with back = item :: deque.back; back_length = deque.back_length + 1};;

    let peek_front deque =
        match deque with
        | { front = []; back = [] } -> raise Empty
        | { front = h :: _} -> h
        | { back = [h] } -> h
        | _ -> failwith "peek_front: Front is empty, back has more than one item";;

    let peek_back deque =
        match deque with
        | { front = []; back = [] } -> raise Empty
        | { back = h :: _} -> h
        | { front = [h] } -> h
        | _ -> failwith "peek_back: Back is empty, front has more than one item";;

    let dequeue_front deque =
        match deque with
        | { front = []; back = [] } -> raise Empty
        | { front = [_]; back = [] } -> empty
        | { front = []; back = [_] } -> empty
        | { front = _ :: t } -> balance { deque with front = t; front_length = deque.front_length - 1 }
        | _ -> failwith "dequeue_front: Front is empty, back has more than one item";;

    let dequeue_back deque =
        match deque with
        | { front = []; back = [] } -> raise Empty
        | { front = [_]; back = [] } -> empty
        | { front = []; back = [_] } -> empty
        | { back = _ :: t } -> balance { deque with back = t; back_length = deque.back_length - 1 }
        | _ -> failwith "dequeue_back: Back is empty, front has more than one item";;

    let peek_front_at deque n =
      let length = deque.front_length + deque.back_length in
      if (n < 0) || (n >= length) then raise InvalidIndex
      else if n < deque.front_length then List.nth deque.front n
      else List.nth deque.back (length - 1 - n);;

      let peek_back_at deque n =
        let length = deque.front_length + deque.back_length in
        if (n < 0) || (n >= length) then raise InvalidIndex
        else if n < deque.back_length then List.nth deque.back n
        else List.nth deque.front (length - 1 - n);;

    let rec set_front_at deque item n =
        if n = 0 then enqueue_front (dequeue_front deque) item
        else enqueue_front (set_front_at (dequeue_front deque) item (n - 1)) (peek_front deque);;

    let rec set_back_at deque item n =
        if n = 0 then enqueue_back (dequeue_back deque) item
        else enqueue_back (set_back_at (dequeue_back deque) item (n - 1)) (peek_back deque);;

end

(* Helper method that produces a deque with a given item in it some number of times *)

let enqueue_duplicate item times =
    let rec aux n deque =
        if n = 0 then deque
        else aux (n - 1) (Deque.enqueue_front deque item) in
    aux times Deque.empty;

module Screen = struct

    open Deque;;

    (* Cursor position is one-based; (1, 1) is the top left, (width, height) is the bottom right. *)

    type t =
    {
        status : string option;
        lines : string Deque.t;
        height : int;
        width : int;
        cursor : int * int;
        needs_scroll : bool;
        needs_more : bool;
        word_wrap : bool;
        pending : string;
        scroll_count : int
    };;

    let make height width =
    {
        status = None;
        lines = enqueue_duplicate (String.make width ' ') height;
        height = height;
        width = width;
        cursor = (1, height);
        needs_scroll = false;
        needs_more = false;
        word_wrap = true;
        pending = "";
        scroll_count = 0
    };;

    let carriage_return screen =
        let (_, y) = screen.cursor in
        if screen.needs_scroll then
            { screen with pending = screen.pending ^ "\n" }
        else if y = screen.height then
            { screen with needs_scroll = true }
        else
            { screen with cursor = (1, y + 1) };;

    let rec print screen text =
        let len = String.length text in
        if len = 0 then
            screen
        else if screen.needs_scroll then
            { screen with pending = screen.pending ^ text }
        else
            let (x, y) = screen.cursor in
            let left_in_line = screen.width - x + 1 in
            if String.contains text '\n' then
                let b = String.index text '\n' in
                let f = String.sub text 0 b in
                let r = String.sub text (b + 1) (len - b - 1) in
                let s1 = print screen f in
                let s2 = carriage_return s1 in
                print s2 r
            else if len >= left_in_line then
                let line = peek_front_at screen.lines (screen.height - y) in
                let over_length_line = (String.sub line 0 (x - 1)) ^ text in
                let b = if screen.word_wrap then
                    let space_location = reverse_index_from over_length_line ' ' (screen.width - 1) in
                    match space_location with None -> (screen.width - 1) | Some location -> location
                else
                    (screen.width - 1) in
                let new_line = (String.sub over_length_line 0 (b + 1)) ^ (String.make (screen.width - b - 1) ' ')  in
                let r = String.sub over_length_line (b + 1) ((String.length over_length_line) - b - 1) in
                let s1 = { screen with lines = set_front_at screen.lines new_line (screen.height - y) } in
                let s2 = carriage_return s1 in
                print s2 r
            else
                let line = peek_front_at screen.lines (screen.height - y) in
                let new_line = (String.sub line 0 (x - 1)) ^ text ^ (String.sub line (x + len - 1) (screen.width - x - len + 1)) in
                { screen with
                    lines = set_front_at screen.lines new_line (screen.height - y);
                    cursor = (x + len, y)
                };;

    let scroll screen =
        let new_screen = { screen with
            lines = enqueue_front (dequeue_back screen.lines) (String.make screen.width ' ');
            cursor = (1, screen.height);
            needs_scroll = false;
            pending = "";
            scroll_count = screen.scroll_count + 1 ;
            } in
        let printed_screen = print new_screen screen.pending in
        { printed_screen with needs_more = printed_screen.needs_scroll && printed_screen.scroll_count >= printed_screen.height - 3 };;

    let rec fully_scroll screen =
        if screen.needs_scroll then fully_scroll (scroll screen)
        else { screen with scroll_count = 0};;

    let set_cursor screen x y =
        { (fully_scroll screen) with cursor = (x, y) };;

    let more screen =
        { screen with lines = set_front_at screen.lines ("[MORE]" ^ (String.make (screen.width - 6) ' ')) 0 };;
end

(* Word-wraps the last line in a list of lines. Assumes that
the tail of the list is already word-wrapped. Returns the
new list. *)

let rec wrap_lines lines line_length =
    let rec aux lines =
        match lines with
        | [] -> []
        | h :: t ->
            let len = String.length h in
            if String.contains h '\n' then
                (* Recursive case 1: there is a break in the last string.
                   Split the string, solve the wrapping problem with no return,
                   and then recurse on the remainder of the string. *)
                let b = String.index h '\n' in
                let f = String.sub h 0 b in
                let r = String.sub h (b + 1) (len - b - 1) in
                let w1 = wrap_lines (f :: t) line_length in
                wrap_lines (r :: w1) line_length
            else if len > line_length then
                (* Recursive case 2: there are no breaks but the line is too long.
                   Find a space to break on, break it, and recurse. *)
                let space_location = reverse_index_from h ' ' line_length in
                let break_point = match space_location with
                    | None -> line_length
                    | Some location -> location in
                aux ((String.sub h (break_point + 1) (len - break_point - 1)) :: (String.sub h 0 break_point) :: t)
            else
                (* Base case: the line has no breaks and is short enough. Do nothing. *)
                lines in
    aux lines;;

(* Takes a list of strings, concatenates a string onto the head, or, if there
is no head, then it becomes the head. *)

let add_to_lines lines str =
    match lines with
    | [] -> [str]
    | h :: t -> (h ^ str) :: t ;;

(* Helper method that takes an item and a function that produces related items.
   The result is the transitive closure of the relation. *)

(* TODO: This is not very efficient because of the call to List.mem in there.
   TODO: A solution involving an immutable set would be more performant for
   TODO: large closures. *)

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

    let make bytes = {
        original_bytes = bytes;
        edits = IntMap.empty
    };;

    let read_byte bytes address =
        let c =
            if IntMap.mem address bytes.edits then IntMap.find address bytes.edits
            else  bytes.original_bytes.[address] in
        int_of_char c;;

    let write_byte bytes address value =
        let byte_of_int value =
            ((value mod 256) + 256 ) mod 256 in
        let b = char_of_int (byte_of_int value) in
        { bytes with edits = IntMap.add address b bytes.edits };;

    let original bytes =
      { bytes with edits = IntMap.empty };;
end

module Memory = struct

    type t =
    {
        dynamic_memory : ImmutableBytes.t;
        static_memory : string;
        static_offset : int
    };;

    let make dynamic static = {
        dynamic_memory = ImmutableBytes.make dynamic;
        static_memory = static;
        static_offset = String.length dynamic
    };;

    let read_byte memory address =
        if address < memory.static_offset then
            ImmutableBytes.read_byte memory.dynamic_memory address
        else
            int_of_char (memory.static_memory.[address - memory.static_offset]);;

    let read_word memory address =
        let high = read_byte memory address in
        let low = read_byte memory (address + 1) in
        256 * high + low;;

    let write_byte memory address value =
      if address >= memory.static_offset then
        failwith "attempt to write static memory"
      else
        let new_memory =
          ImmutableBytes.write_byte memory.dynamic_memory address value in
        { memory with dynamic_memory = new_memory };;

    let write_word memory address value =
        let w = unsigned_word value in
        let high = w lsr 8 in
        let low = w land 0xFF in
        let first = write_byte memory address high in
        write_byte first (address + 1) low;;

    let original memory =
      let original_bytes = ImmutableBytes.original memory.dynamic_memory in
      { memory with dynamic_memory = original_bytes };;
end

module Story = struct
    type t =
    {
        memory : Memory.t
    };;

    (* *)
    (* Dealing with memory *)
    (* *)

    let original story =
      { memory = Memory.original story.memory };;

    let fetch_bit n word =
        (word land (1 lsl n)) lsr n = 1;;

    let clear_bit n word =
        word land (lnot (1 lsl n));;

    let set_bit n word =
        word lor (1 lsl n);;

    let set_bit_to n word value =
        if value then set_bit n word
        else clear_bit n word;;

    let fetch_bits high length word =
        let mask = lnot (-1 lsl length) in
        (word lsr (high - length + 1)) land mask;;

    (* A "word address" is only used in the abbreviation table, and is always
    just half the real address. A "packed address" is used in calls and fetching
    strings, and is half the real address in v3 but different for other versions. *)

    let decode_word_address word_address =
        word_address * 2;;

    (* TODO: only works for v3 *)
    let decode_packed_address story packed =
        packed * 2;;

    let read_word story address =
        Memory.read_word story.memory address;;

    let read_byte story address =
        Memory.read_byte story.memory address;;

    let write_word story address value =
        { memory = Memory.write_word story.memory address value };;

    let write_byte story address value =
        { memory = Memory.write_byte story.memory address value };;

    (* Writes a series of bytes into memory. Does not zstring encode them. *)
    let write_string story address text =
        let length = String.length text in
        let rec aux i s =
            if i = length then s
            else aux (i + 1) (write_byte s (address + i) (int_of_char text.[i])) in
        let copied = aux 0 story in
        write_byte copied (address + length) 0;;

    (* Debugging method for displaying a raw block of memory. *)
    let display_bytes story address length =
        let blocksize = 16 in
        let rec print_loop i acc =
            if i = length then
                acc
            else (
                let s =
                    if i mod blocksize = 0 then
                        Printf.sprintf "\n%06x: " (i + address)
                    else
                        "" in
                let s2 = Printf.sprintf "%02x " (read_byte story (i + address)) in
            print_loop (i + 1) (acc ^ s ^ s2)) in
        (print_loop 0 "") ^ "\n";;

    (* *)
    (* Header *)
    (* *)

    (* TODO: Header features beyond v3 *)

    let header_size = 64;;
    let version_offset = 0;;
    let version story =
        read_byte story version_offset;;

    let flags1 story =
        let flags1_offset = 1 in
        read_byte story flags1_offset;;

    type status_line_kind_type = NoStatus | ScoreStatus | TimeStatus;;

    let status_line_kind story =
        match (version story, fetch_bit 1 (flags1 story))  with
        | (1, _)
        | (2, _)
        | (3, false) -> ScoreStatus
        | (3, true) -> TimeStatus
        | _ -> NoStatus;;

    (* TODO: More Flags 1 *)

    let high_memory_base story =
        let high_memory_base_offset = 4 in
        read_word story high_memory_base_offset;;

    let initial_program_counter story =
        let initial_program_counter_offset = 6 in
        read_word story initial_program_counter_offset;;

    let dictionary_base story =
        let dictionary_base_offset = 8 in
        read_word story dictionary_base_offset;;

    let object_table_base story =
        let object_table_base_offset = 10 in
        read_word story object_table_base_offset;;

    let global_variables_table_base story =
        let global_variables_table_base_offset = 12 in
        read_word story global_variables_table_base_offset ;;

    let static_memory_base_offset = 14;;
    let static_memory_base story =
        read_word story static_memory_base_offset ;;

    let flags2 story =
      let flags2_offset = 16 in
      read_byte story flags2_offset;;

    let get_transcript_flag story =
      let transcript_bit = 0 in
      fetch_bit transcript_bit (flags2 story);;

    let set_transcript_flag story value =
      let flags2_offset = 16 in
      let transcript_bit = 0 in
      let new_flags2 = (set_bit_to transcript_bit (flags2 story) value) in
      write_byte story flags2_offset new_flags2;;

    let abbreviations_table_base story =
        let abbreviations_table_base_offset = 24 in
        read_word story abbreviations_table_base_offset ;;

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
        if n < 0 || n >= abbreviation_table_length then
            failwith "bad offset into abbreviation table";
        decode_word_address (read_word story ((abbreviations_table_base story) + (n * 2)));;

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
        10-bit character. *)

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

        let rec aux acc mode1 current_address =
            let zchar_bit_size = 5 in
            let word = read_word story current_address in
            let is_end = fetch_bit 15 word in
            let zchar1 = fetch_bits 14 zchar_bit_size word in
            let zchar2 = fetch_bits 9 zchar_bit_size word in
            let zchar3 = fetch_bits 4 zchar_bit_size word in
            let (text1, mode2) = process_zchar zchar1 mode1 in
            let (text2, mode3) = process_zchar zchar2 mode2 in
            let (text3, mode_next) = process_zchar zchar3 mode3 in
            let new_acc = acc ^ text1 ^ text2 ^ text3 in
            if is_end then new_acc
            else aux new_acc mode_next (current_address + 2) in
        aux "" (Alphabet 0) address;;

    (* A debugging method for looking at memory broken up into the
    1 / 5 / 5 / 5 bit chunks used by zstrings. *)

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
    let default_property_table_size story =
        31;;

    let default_property_table_entry_size = 2;;

    let default_property_table_base = object_table_base;;

    (* TODO: The spec implies that default properties
       are numbered starting at 1; is this right? *)
    let default_property_value story n =
        if n < 1 || n > (default_property_table_size story) then failwith "invalid index into default property table"
        else  read_word story ((default_property_table_base story) + (n - 1) * default_property_table_entry_size);;

    (* A debugging method for looking at the default property table *)
    let display_default_property_table story =
        let rec display_loop i acc =
            if i > (default_property_table_size story) then acc
            else (
                let s = Printf.sprintf "%02x: %04x\n" i (default_property_value story i) in
                display_loop (i + 1) (acc ^ s)) in
        display_loop 1 "";;

    let object_tree_base story =
        (default_property_table_base story) + default_property_table_entry_size * (default_property_table_size story);;

    (* TODO: Object table entry is larger in version 4 *)
    let object_table_entry_size = 9;;

    (* Oddly enough, the Z machine does not ever say how big the object table is.
       Assume that the address of the first property block in the first object is
       the bottom of the object tree table. *)

    let object_address story object_number =
        (object_tree_base story) + (object_number - 1) * object_table_entry_size;;

    let object_attributes_word_1 story object_number =
        read_word story (object_address story object_number);;

    let object_attributes_word_2 story object_number =
        let attributes2_offset = 2 in
        read_word story ((object_address story object_number) + attributes2_offset);;

    let attribute_count = 32;;
    (* TODO: 48 attributes in version 4 *)

    let object_attribute_address story object_number attribute_number =
        if attribute_number < 0 || attribute_number >= attribute_count then
            failwith "bad attribute";
        let offset = attribute_number / 8 in
        let address = (object_address story object_number) + offset in
        let bit = 7 - (attribute_number mod 8) in
        (address, bit);;

    let object_attribute story object_number attribute_number =
        let (address, bit) = object_attribute_address story object_number attribute_number in
        let byte = read_byte story address in
        fetch_bit bit byte;;

    let set_object_attribute story object_number attribute_number =
        let (address, bit) = object_attribute_address story object_number attribute_number in
        let byte = read_byte story address in
        write_byte story address (set_bit bit byte);;

    let clear_object_attribute story object_number attribute_number =
        let (address, bit) = object_attribute_address story object_number attribute_number in
        let byte = read_byte story address in
        write_byte story address (clear_bit bit byte);;

    let object_parent_offset = 4;;

    let object_parent story object_number =
        read_byte story ((object_address story object_number) + object_parent_offset);;

    let set_object_parent story object_number new_parent =
        write_byte story ((object_address story object_number) + object_parent_offset) new_parent;;

    let object_sibling_offset = 5;;

    let object_sibling story object_number =
        read_byte story ((object_address story object_number) + object_sibling_offset);;

    let set_object_sibling story object_number new_sibling =
        write_byte story ((object_address story object_number) + object_sibling_offset) new_sibling;;

    let object_child_offset = 6;;

    let object_child story object_number =
        read_byte story ((object_address story object_number) + object_child_offset);;

    let set_object_child story object_number new_child =
        write_byte story ((object_address story object_number) + object_child_offset) new_child;;

    let object_property_offset = 7;;

    let object_property_address story object_number =
        read_word story ((object_address story object_number) + object_property_offset);;

    let object_count story =
        ((object_property_address story 1) - (object_tree_base story)) / object_table_entry_size;;

    let object_name story n =
        let length = read_byte story (object_property_address story n) in
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

    let invalid_object = 0;;

    (* Takes a child object and detatches it from its parent *)

    let remove_object story child =
        let original_parent = object_parent story child in
        if original_parent = invalid_object then
            story
        else
            let edit1 = (
                if child = object_child story original_parent then
                    set_object_child story original_parent (object_sibling story child)
                else
                    set_object_sibling story (find_previous_sibling story child) (object_sibling story child)) in
            set_object_parent edit1 child invalid_object;;

    (* Takes a child object and a parent object, and causes the child to be the
    first child of the parent. *)

    let insert_object story child parent =
        (* Detatch the new child from its old parent *)
        let edit1 = remove_object story child in
        (* Hook up the new child to its new parent *)
        let edit2 = set_object_parent edit1 child parent in
        (* Hook up the sibling chain *)
        let edit3 = set_object_sibling edit2 child (object_child edit2 parent) in
        (* Make the child the new first child of the parent *)
        set_object_child edit3 parent child;;

    (* Produces a list of (number, length, address) tuples *)
    let property_addresses story object_number =
        let rec aux acc address =
            let b = read_byte story address in
            if b = 0 then
                acc
            else
                let property_length = (fetch_bits 7 3 b) + 1 in
                let property_number = (fetch_bits 4 5 b) in
                aux ((property_number, property_length, address + 1) :: acc) (address + 1 + property_length) in
        let property_header_address = object_property_address story object_number in
        let property_name_word_length = read_byte story property_header_address in
        let first_property_address = property_header_address + 1 + property_name_word_length * 2 in
        aux [] first_property_address;;

    (* Takes the address of a property data block, returns the length.
    The length is always in the top three bits of the byte before the block. *)

    let property_length_from_address story address =
        if address = 0 then 0
        else 1 + (fetch_bits 7 3 (read_byte story (address - 1)));;

    let property_address story object_number property_number =
        let rec aux addresses =
            match addresses with
            | [] -> 0
            | (number, _, address) :: tail -> if number = property_number then address else aux tail in
        aux (property_addresses story object_number);;

    (* Fetch the one or two byte value associated with a given property of a given object.
    If the object does not have that property then fetch the default property value. *)
    let object_property story object_number property_number =
        let rec aux addresses =
            match addresses with
            | [] -> default_property_value story property_number
            | (number, length, address) :: tail ->
                if number = property_number then (
                    if length = 1 then
                        read_byte story address
                    else if length = 2 then
                        read_word story address
                    else
                        failwith "bad property length")
                else
                    aux tail in
        aux (property_addresses story object_number);;

    (* Given a property number, find the first property of an object
    greater than it. Note that this assumes that properties are enumerated in
    order by property_addresses. Returns zero if there is no such property. *)
    let get_next_property story object_number property_number =
        let rec aux addrs =
            match addrs with
            | [] -> 0
            | (number, _, _) :: tail -> if number > property_number then number else aux tail in
        aux (property_addresses story object_number);;

    (* Writes a one or two byte property associated with a given object. *)
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

    (* Debugging method for displaying the property numbers and values for a given object *)
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

    (* Count down all the objects in the object table and record which ones have no parent. *)
    let object_roots story =
        let rec aux object_number acc =
            if object_number = invalid_object then acc
            else if (object_parent story object_number) = invalid_object then aux (object_number - 1) (object_number :: acc)
            else aux (object_number - 1) acc in
        aux (object_count story) [];;

    let display_object_tree story =
        let rec aux acc indent i =
            if i = invalid_object then acc
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
        read_byte story (dictionary_base story);;

    let word_separators story =
        let base = dictionary_base story in
        let count = read_byte story base in
        let rec aux acc i =
            if i < 1 then acc
            else aux ((read_byte story (base + i)) :: acc) (i - 1) in
        aux [] count;;

    let dictionary_entry_length story =
        read_byte story ((dictionary_base story) + (word_separators_count story) + 1);;

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

    (* Binary search a range. Min is inclusive, max is exclusive. *)
    let rec binary_search min max compare =
      if min >= max then
        None
      else
        let middle = (min + max) / 2 in
        let comparison = compare middle in
        if comparison < 0 then binary_search (middle + 1) max compare
        else if comparison > 0 then binary_search min middle compare
        else Some middle;;

    (* Takes a string and finds the address of the corresponding zstring in the dictionary *)
    (* Note this is the address of the dictionary string, not the dictionary entry number. *)
    let dictionary_lookup story text =
      let count = dictionary_entry_count story in
      let truncated =
        if (String.length text) > dictionary_max_word_length then
          String.sub text 0 dictionary_max_word_length
        else
          text in
      let compare i = String.compare (dictionary_entry story i) truncated in
      match binary_search 0 count compare with
      | None -> 0
      | Some entry_index -> dictionary_entry_address story entry_index;;

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
            | Small_operand :: types -> (Small (read_byte story operand_address))  :: (decode_operands (operand_address + 1) types)
            | Variable_operand :: types -> (Variable (decode_variable (read_byte story operand_address))) :: (decode_operands (operand_address + 1) types)
            | Omitted :: _ -> failwith "omitted operand type passed to decode operands" in

        let rec operand_size operand_types =
            match operand_types with
            | [] -> 0
            | Large_operand :: types -> 2 + (operand_size types)
            | _ :: types -> 1 + (operand_size types) in

        (* Spec 4.7 *)

        let branch_size branch_code_address =
            let b = (read_byte story branch_code_address) in
            if (fetch_bit 6 b) then 1 else 2 in

        let decode_branch branch_code_address total_length =
            let b = (read_byte story branch_code_address) in
            let sense = (fetch_bit 7 b) in
            let bottom6 = fetch_bits 5 6 b in
            let offset =
                if (fetch_bit 6 b) then
                    bottom6
                else
                    let unsigned = 256 * bottom6 + (read_byte story (branch_code_address + 1)) in
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
                | (Large large) :: tail -> (Large (decode_packed_address story large)) :: tail
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

        let b = read_byte story address in

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
        | Variable_form -> decode_variable_types (read_byte story (address + opcode_length)) in

        let type_length = (match form with Variable_form -> 1 | _ -> 0) in
        let operands = decode_operands (address + opcode_length + type_length) operand_types in
        let operand_length = operand_size operand_types in
        let store_address = address + opcode_length + type_length + operand_length in
        let store = if has_store opcode then Some (decode_variable (read_byte story store_address)) else None in
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

    (* Suppose an instruction either has a branch portion to an address,
    or a jump to an address. What is that address? *)

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

    (* Any given instruction in a routine either goes on to the next instruction,
    when it is done, or branches to another instruction when it is done, or terminates
    the routine. Given the address of an instruction, what are all the reachable instructions
    in this routine? Note that this could miss instructions if a jump is made to a location
    read from a variable. *)

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
        let c = read_byte story routine_address in
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
    (* Again, this can miss routines that are called with a variable as the address. *)

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

    (* TODO: Have this return a string? Will want to break pure functionality here and use a mutable buffer
    because this can get long. *)
    let display_all_routines story =
        List.iter (fun r -> Printf.printf "\n---\n%s" (display_routine story r)) (all_routines story);;

    let first_global = 16;;
    let current_object_global = 16;;
    let current_score_global = 17;;
    let turn_count_global = 18;;
    let current_hours_global = 17;;
    let current_minute_global = 18;;

    let last_global = 255;;

    (* Note that globals are indexed starting at 16 *)
    let read_global story global_number =
        if global_number < first_global || global_number > last_global then
            failwith "global variable index out of range";
        read_word story ((global_variables_table_base story) + (global_number - first_global) * 2)

    let write_global story global_number value =
        if global_number < first_global || global_number > last_global then
            failwith "global variable index out of range";
        write_word story ((global_variables_table_base story) + (global_number - first_global) * 2) value;;

    let current_object_name story =
        let current_object = read_global story current_object_global in
        if current_object = invalid_object then ""
        else object_name story current_object;;

    let status_globals story =
        (signed_word (read_global story current_score_global), read_global story turn_count_global);;
end (* Story *)

module Interpreter = struct

    open Story;;
    open Screen;;

    type state =
        | Running
        | Waiting_for_input
        | Halted;;

    type frame =
    {
        stack : int list;
        locals : int IntMap.t;
        called_from : int;
        called : int
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
        state : state;

        (* output stream 1 *)
        screen : Screen.t;
        has_new_output : bool;
        screen_selected : bool;

        (* output stream 2 *)
        transcript : string list;
        transcript_selected : bool;

        (* TODO: output stream 3 NYI *)

        (* output stream 4 *)
        commands : string list;
        commands_selected : bool;

        (* TODO: Other input streams *)
        input : string;
        input_max : int;
    };;

    let make story screen =

    (* TODO: Need to set appropriate words for screen size *)
    let pc = initial_program_counter story in
    {
        story = story;
        program_counter = pc;
        frames = [ { stack = []; locals = IntMap.empty; called_from = 0; called = pc } ];
        (* TODO: Seed these randomly *)
        random_w = Int32.of_int 123;
        random_x = Int32.of_int 123;
        random_y = Int32.of_int 123;
        random_z = Int32.of_int 123;
        state = Running;
        screen = screen;
        has_new_output = false;
        screen_selected = true;
        transcript = [""];
        transcript_selected = get_transcript_flag story;
        commands = [];
        commands_selected = false;
        input = "";
        input_max = 0
    };;

    let current_frame interpreter =
        List.hd interpreter.frames;;

    let add_frame interpreter frame =
        { interpreter with frames = frame :: interpreter.frames };;

    let remove_frame interpreter =
        { interpreter with frames = List.tl (interpreter.frames) };;

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

    let set_program_counter interpreter new_program_counter =
        { interpreter with program_counter = new_program_counter };;

    (* Reading operands can change the state of the interpreter, because it can
       pop the stack. *)

    let read_operand_no_pop interpreter operand =
        match operand with
        | Large large -> large
        | Small small -> small
        | Variable Stack -> peek_stack interpreter
        | Variable Local local -> IntMap.find local (current_frame interpreter).locals
        | Variable Global global -> read_global interpreter.story global;;

    let read_operand interpreter operand =
        let value = read_operand_no_pop interpreter operand in
        match operand with
        | Variable Stack -> (value, pop_stack interpreter)
        | _ -> (value, interpreter);;

    let write_local interpreter local value =
        match interpreter.frames with
        | h :: t -> { interpreter with frames = ({ h with locals = IntMap.add local value h.locals }) :: t }
        | _ -> failwith "frame set is empty"

    let write_global interpreter global value =
        { interpreter with story = write_global interpreter.story global value };;

    let do_store interpreter variable value =
        match variable with
        | Local local -> write_local interpreter local value
        | Global global -> write_global interpreter global value
        | Stack -> push_stack interpreter value;;

    (* Note that we have a rare case of mutually recursive functions here; OCaml tends
       to encourage directly recursive functions but not so much mutually recursive functions.

       Z-machine instructions essentially have three parts. The first part evaluates the
       operands. The second part either causes a side effect or computes a result.
       The third part stores the result and computes what instruction to run next.

       Almost all instructions do the three parts in that order. Calls and returns are
       the weird ones. A call does the first part -- the operands are evaluated. But
       the back half of the call -- the production of the result and the decision about
       where to go after the call -- are executed by the corresponding return.

       That's why we need this code to be mutually recursive. The branch in the third
       half of the instruction can do a return, so it needs to be able to return. But
       the return needs to be able to handle the storage of the result *in the context
       of the call instruction we are returning to*, and then handle the branch to
       the instruction after the call. *)

    let rec handle_branch interpreter instruction result =
        let next_instruction () =
            set_program_counter interpreter (interpreter.program_counter + instruction.length) in
        match instruction.branch with
        | None -> next_instruction ()
        | Some (sense, Return_false) ->
            if (result <> 0) = sense then handle_return interpreter instruction 0
            else next_instruction ()
        | Some (sense, Return_true) ->
            if (result <> 0) = sense then handle_return interpreter instruction 1
            else next_instruction ()
        | Some (sense, Branch_address branch_target) ->
            if (result <> 0) = sense then set_program_counter interpreter branch_target
            else next_instruction ()

    and handle_store_and_branch interpreter instruction result =
        let store_interpreter =
            match instruction.store with
            | None -> interpreter
            | Some variable -> do_store interpreter variable result in
        handle_branch store_interpreter instruction result

    and handle_return interpreter instruction value =
        let next_program_counter = (current_frame interpreter).called_from in
        let result_interpreter = set_program_counter (remove_frame interpreter) next_program_counter in
        let call_instr = decode_instruction result_interpreter.story next_program_counter in
        handle_store_and_branch result_interpreter call_instr value;;

    (*
        Always evaluate the operand -- we might be popping the stack
        If the local number is valid then update the locals map with
        the argument. *)

    (* There can be more or fewer arguments than there are locals; we have to deal
    with both cases. *)


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

    type output_stream_kind =
    | ScreenStream
    | TranscriptStream
    | MemoryStream
    | CommandStream;;

    let select_output_stream interpreter stream value =
      match stream with
      | ScreenStream -> { interpreter with screen_selected = value }
      | TranscriptStream -> { interpreter with
        transcript_selected = value;
        story = set_transcript_flag interpreter.story value }
      | MemoryStream -> failwith "TODO: output stream 3 NYI"
      | CommandStream -> { interpreter with commands_selected = value };;

    let add_to_transcript transcript text =
      let transcript_width = 80 in
      wrap_lines (add_to_lines transcript text) transcript_width;;

    let interpreter_print interpreter text =
      let new_transcript =
        if interpreter.transcript_selected then
          add_to_transcript interpreter.transcript text
        else interpreter.transcript in
      let new_screen =
        if interpreter.screen_selected then Screen.print interpreter.screen text
        else interpreter.screen in
      { interpreter with
        transcript = new_transcript;
        screen = new_screen;
        has_new_output = interpreter.screen_selected };;

    let set_status_line interpreter =
        let object_name () =
            current_object_name interpreter.story in
        let build_status_line right =
            let right_length = String.length right in
            let left = object_name() in
            let left_length = String.length left in
            let width = interpreter.screen.width in
            let left_trimmed =
                if left_length + right_length < width then left
                else String.sub left 0 (width - right_length - 1) in (* TODO: Assumes that width >= right_length *)
            left_trimmed ^ (String.make (width - right_length - (String.length left_trimmed)) ' ') ^ right in
        let time_status () =
            let (hours, minutes) = status_globals interpreter.story in
            let suffix = if hours >= 12 then "PM" else "AM" in
            let adjusted_hours = (hours mod 12) + 12 in
            build_status_line (Printf.sprintf "%d:%02d%s" adjusted_hours minutes suffix) in
        let score_status () =
            let (score, turns) = status_globals interpreter.story in
            build_status_line (Printf.sprintf "%d/%d" score turns) in
        match status_line_kind interpreter.story with
        | NoStatus -> interpreter
        | TimeStatus -> { interpreter with
            has_new_output = true;
            screen = { interpreter.screen with status = Some (time_status()) } }
        | ScoreStatus -> { interpreter with
            has_new_output = true;
            screen = { interpreter.screen with status = Some (score_status()) } };;

    let complete_sread interpreter instruction input =

        (* TODO: Get word separator list from story *)

        let tokenise text interp =
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

        let running_interpreter = { interpreter with state = Running } in

        let (text_address, parse_address, operands_interpreter) =
            match instruction.operands with
                | [x_operand; y_operand] ->
                    let (x, x_interpreter) = read_operand running_interpreter x_operand in
                    let (y, y_interpreter) = read_operand x_interpreter y_operand in
                    (x, y, y_interpreter)
           | _ -> failwith (Printf.sprintf "instruction at %04x must have two operands" instruction.address ) in

        let text = String.lowercase input in

        let maximum_letters = read_byte operands_interpreter.story text_address in

        let trimmed = if (String.length text) > maximum_letters then String.sub text 0 maximum_letters else text in
        let string_copied_interpreter = { operands_interpreter with story = write_string operands_interpreter.story (text_address + 1) trimmed } in

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

        let commands_interpreter =
          if string_copied_interpreter.commands_selected then
            {string_copied_interpreter with
              commands = input :: string_copied_interpreter.commands }
          else string_copied_interpreter in
        let printed_interpreter =
          interpreter_print commands_interpreter (input ^ "\n") in
        let new_screen_interpreter = { printed_interpreter with
          screen = fully_scroll printed_interpreter.screen } in

        (*
        Next, lexical analysis is performed on the text (except that in Versions 5 and later, if parsebuffer
        is zero then this is omitted). Initially, byte 0 of the parse-buffer should hold the maximum
        number of textual words which can be parsed. (If this is n, the buffer must be at least 2 +
        4*n bytes long to hold the results of the analysis.)
        *)

        let maximum_parse = read_byte new_screen_interpreter.story parse_address in

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

        let tokens = tokenise trimmed new_screen_interpreter in

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

        let (count, tokens_written_interpreter) =  write_tokens tokens (parse_address + 2) 0 new_screen_interpreter in

        (* TODO: Make a write byte that takes interpreters *)

        let length_copied_interpreter = { tokens_written_interpreter with story = write_byte tokens_written_interpreter.story (parse_address + 1) count } in

        handle_store_and_branch length_copied_interpreter instruction 0;;

    let handle_sread interpreter instruction =

        (* This instruction is broken up into two halves. The first determines the size of
        the text buffer needed and then gives back an interpreter set to "I need input".
        The second half does the actual work once the host has provided the data.

        Note that we are doing something unusual here. We potentially pop two values
        off the stack, but we discard the mutated interpreter state. We will simply
        compute the values again in the original interpreter on the completion side
        of the instruction! Immutable data structures for the win! *)

        let (text_address, _) =
            match instruction.operands with
            | [x_operand; y_operand] -> read_operand interpreter x_operand
            | _ -> failwith (Printf.sprintf "instruction at %04x must have two operands" instruction.address ) in

        (* SPEC

        This opcode reads a whole command from the keyboard (no prompt is automatically displayed).

        It is legal for this to be called with the cursor at any position on any window.

        In Versions 1 to 3, the status line is automatically redisplayed first.

        *)

        let status_interpreter = set_status_line interpreter in

        (* SPEC

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

        let maximum_letters = read_byte status_interpreter.story text_address in

        (*
        Interpreters are asked to halt with a suitable error message if the text or parse buffers have
        length of less than 3 or 6 bytes, respectively: this sometimes occurs due to a previous array being
        overrun, causing bugs which are very difficult to find.
        *)

        if maximum_letters < 3 then failwith "bad text buffer in sread";

        (* TODO: At this point set the state to "needs input" and return that interpreter.
        The host will get the input and call back to complete the process. *)

        { status_interpreter with
            state = Waiting_for_input ;
            input_max = maximum_letters } ;;

    let step_instruction interpreter =
        let instruction = decode_instruction interpreter.story interpreter.program_counter in

        (* Some helper routines for generic instructions that simply evaluate operands,
           compute a result from them, store, and branch. *)

        let handle_op0 compute_result =
            let (result, result_interpreter) = compute_result interpreter in
            handle_store_and_branch result_interpreter instruction result in

        let handle_op0_effect compute_effect =
          handle_op0 (fun i -> (0, compute_effect i)) in

        (* TODO: No zero-operand instructions are useful for their values.
        Consider simplifying this code.
        let handle_op0_value compute_value =
          handle_op0 (fun i -> (compute_value i, i)) in
*)

        let handle_op1 compute_result =
            match instruction.operands with
            | [x_operand] ->
                let (x, operand_interpreter) = read_operand interpreter x_operand in
                let (result, result_interpreter) = compute_result x operand_interpreter in
                handle_store_and_branch result_interpreter instruction result
           | _ -> failwith "instruction must have one operand" in

        let handle_op1_effect compute_effect =
          handle_op1 (fun x i -> (0, compute_effect x i)) in

        let handle_op1_value compute_value =
          handle_op1 (fun x i -> (compute_value x i, i)) in

        let handle_op2 compute_result =
            match instruction.operands with
            | [x_operand; y_operand] ->
                let (x, x_interpreter) = read_operand interpreter x_operand in
                let (y, y_interpreter) = read_operand x_interpreter y_operand in
                let (result, result_interpreter) = compute_result x y y_interpreter in
                handle_store_and_branch result_interpreter instruction result
           | _ -> failwith (Printf.sprintf "instruction at %04x must have two operands" instruction.address ) in

        let handle_op2_effect compute_effect =
          handle_op2 (fun x y i -> (0, compute_effect x y i)) in

        let handle_op2_value compute_value =
          handle_op2 (fun x y i -> (compute_value x y i, i)) in

        let handle_op3 compute_result =
          match instruction.operands with
          | [x_operand; y_operand; z_operand] ->
            let (x, x_interpreter) = read_operand interpreter x_operand in
            let (y, y_interpreter) = read_operand x_interpreter y_operand in
            let (z, z_interpreter) = read_operand y_interpreter z_operand in
            let (result, result_interpreter) = compute_result x y z z_interpreter in
            handle_store_and_branch result_interpreter instruction result
          | _ -> failwith (Printf.sprintf "instruction at %04x must have three operands" instruction.address ) in

        let handle_op3_effect compute_effect =
          handle_op3 (fun x y z i -> (0, compute_effect x y z i)) in

        let handle_op3_value compute_value =
          handle_op3 (fun x y z i -> (compute_value x y z i, i)) in

        let handle_op4 compute_result =
          match instruction.operands with
          | [w_operand; x_operand; y_operand; z_operand] ->
            let (w, w_interpreter) = read_operand interpreter w_operand in
            let (x, x_interpreter) = read_operand w_interpreter x_operand in
            let (y, y_interpreter) = read_operand x_interpreter y_operand in
            let (z, z_interpreter) = read_operand y_interpreter z_operand in
            let (result, result_interpreter) = compute_result w x y z z_interpreter in
            handle_store_and_branch result_interpreter instruction result
           | _ -> failwith (Printf.sprintf "instruction at %04x must have four operands" instruction.address ) in

(*         let handle_op4_effect compute_effect =
           handle_op4 (fun w x y z i -> (0, compute_effect w x y z i)) in *)

        let handle_op4_value compute_value =
          handle_op4 (fun w x y z i -> (compute_value w x y z i, i)) in

        let handle_jl x y interp =
          if (signed_word x) < (signed_word y) then 1 else 0 in
        let handle_jg x y interp =
          if (signed_word x) > (signed_word y) then 1 else 0 in
        let handle_jin x y interp =
          if (object_parent interp.story x) = y then 1 else 0 in
        let handle_test x y interp =
          let x = unsigned_word x in
          let y = unsigned_word y in
          if (x land y) = y then 1 else 0 in
        let handle_or x y interp =
          (unsigned_word x) lor (unsigned_word y) in
        let handle_and x y interp =
          (unsigned_word x) land (unsigned_word y) in
        let handle_test_attr obj attr interp =
          if object_attribute interp.story obj attr then 1 else 0 in
        let handle_set_attr obj attr interp =
          { interp with story = set_object_attribute interp.story obj attr } in
        let handle_clear_attr obj attr interp =
          { interp with story = clear_object_attribute interp.story obj attr } in
        let handle_insert_obj child parent interp =
          { interp with story = insert_object interp.story child parent } in
        let handle_loadw arr ind interp =
          read_word interp.story (arr + ind * 2) in
        let handle_loadb arr ind interp =
          read_byte interp.story (arr + ind) in
        let handle_get_prop obj prop interp =
          object_property interp.story obj prop in
        let handle_get_prop_addr obj prop interp =
          property_address interp.story obj prop in
        let handle_get_next_prop obj prop interp =
          get_next_property interp.story obj prop in
        let handle_add x y interp =
          signed_word (x + y)  in
        let handle_sub x y interp =
          signed_word (x - y) in
        let handle_mul x y interp =
          signed_word (x * y) in
        let handle_div x y interp =
          signed_word (x / y) in
        let handle_mod x y interp =
          signed_word (x mod y) in
        let handle_jz x interp =
          if x = 0 then 1 else 0 in
        let handle_get_sibling obj interp =
          object_sibling interp.story obj in
        let handle_get_child obj interp =
          object_child interp.story obj in
        let handle_get_parent obj interp =
          object_parent interp.story obj in
        let handle_get_prop_len x interp =
          property_length_from_address interp.story x in
        let handle_print_addr x interp =
          interpreter_print interp (read_zstring interp.story x) in
        let handle_remove_obj x interp =
          { interp with story = remove_object interp.story x} in
        let handle_print_obj x interp =
          interpreter_print interp (object_name interp.story x) in
        let handle_print_paddr paddr interp =
          let addr = decode_packed_address interp.story paddr in
          let text = read_zstring interp.story addr in
          interpreter_print interp text in
        let handle_load x interp =
          x in
        let handle_not x interp =
          unsigned_word (lnot x) in
        let handle_nop interp =
          interp in
        let handle_restart () =
          (* If transcripting is active, this has to stay on in
          the restarted interpreter *)
          (* TODO: windowed screens might need work here *)
          let transcript_on = interpreter.transcript_selected in
          let transcript = interpreter.transcript in
          let commands = interpreter.commands in
          let story = original interpreter.story in
          let original = make story interpreter.screen in
          let restarted_interpreter = select_output_stream original TranscriptStream transcript_on in
          { restarted_interpreter with transcript = transcript; commands = commands } in
        let handle_storew arr ind value interp =
          { interp with story = write_word interp.story (arr + ind * 2) value } in
        let handle_storeb arr ind value interp =
          { interp with story = write_byte interp.story (arr + ind) value } in
        let handle_putprop obj prop value interp =
          { interp with story = write_property interp.story obj prop value } in
        let handle_print_char x interp =
          interpreter_print interp (Printf.sprintf "%c" (char_of_int x)) in
        let handle_print_num x interp =
          interpreter_print interp (Printf.sprintf "%d" (signed_word x)) in
        let handle_push x interp =
          push_stack interp x in
        let handle_output_stream stream interp =
          (* TODO: This is a variadic instruction *)
          let new_interpreter = match stream with
          | 0 -> interp
          | 1 -> select_output_stream interp ScreenStream true
          | -1 -> select_output_stream interp ScreenStream false
          | 2 -> select_output_stream interp TranscriptStream true
          | -2 -> select_output_stream interp TranscriptStream true
          | 3
          | -3 -> failwith "TODO: selecting/deselecting stream 3 is NYI "
          | 4 -> select_output_stream interp CommandStream true
          | -4 -> select_output_stream interp CommandStream true
          | _ -> failwith "Invalid stream in output_stream" in
          new_interpreter in
        let handle_pop interp =
          pop_stack interp in
        let handle_new_line interp =
          interpreter_print interp "\n" in
        let handle_show_status interp =
          set_status_line interp in
        let handle_print interp =
          match instruction.text with
          | Some text -> interpreter_print interp text
          | _ -> failwith "no text in print instruction" in
        let handle_random n interp =
            let random_next () =
                (* See wikipedia article on xorshift *)
                let t = Int32.logxor interp.random_x (Int32.shift_left interp.random_x 11) in
                let new_x = interp.random_y in
                let new_y = interp.random_z in
                let new_z = interp.random_w in
                let new_w = Int32.logxor (Int32.logxor (Int32.logxor interp.random_w (Int32.shift_right_logical interp.random_w 19)) t) (Int32.shift_right_logical t 8) in
                let result = 1 + (Int32.to_int (Int32.rem new_w (Int32.of_int n)) + n) mod n in
                (result, { interp with random_w = new_w; random_x = new_x; random_y = new_y; random_z = new_z }) in
            if n = 0 then
                (0, { interp with random_w = (Random.self_init(); Random.int32 (Int32.of_int 1000000)) })
            else if n < 0 then
                (0, { interp with random_w = Int32.of_int n; random_x = Int32.of_int 123; random_y = Int32.of_int 123; random_z = Int32.of_int 123 })
            else
                random_next() in

        (* Some helpers for instructions that are a bit unusual, like returns *)

        (* For an unconditional jump we might as well just evaluate the operand and branch directly. *)

        let handle_jump () =
            match instruction.operands with
            | [target_operand] ->
                let (target, target_interpreter) = read_operand interpreter target_operand in
                set_program_counter target_interpreter target
            | _ -> failwith "instruction must have one operand" in

        (* je is interesting in that it is a 2OP that can take 2 to 4 operands. *)
        let handle_je () =
            let handle_je2 test x interp =
              if (signed_word test) = (signed_word x) then 1 else 0 in
            let handle_je3 test x y interp =
              let test = signed_word test in
              let x = signed_word x in
              let y = signed_word y in
              if test = x || test = y then 1 else 0 in
            let handle_je4 test x y z interp =
              let test = signed_word test in
              let x = signed_word x in
              let y = signed_word y in
              let z = signed_word z in
              if test = x || test = y || test = z then 1 else 0 in
            match instruction.operands with
            | [_; _] -> handle_op2_value handle_je2
            | [_; _; _] -> handle_op3_value handle_je3
            | [_; _; _; _] -> handle_op4_value handle_je4
            | _ -> failwith "je instruction requires 2 to 4 operands" in

        (* Do not advance to the next instruction *)
        let handle_quit () =
            { interpreter with state = Halted } in

        let handle_print_ret () =
            let printed_interpreter =
                (match instruction.text with
                | Some text -> interpreter_print interpreter (text ^ "\n")
                | _ -> failwith "no text in print_ret instruction") in
            handle_return printed_interpreter instruction 1 in

        let handle_ret_popped () =
            handle_return (pop_stack interpreter) instruction (peek_stack interpreter) in

        let handle_ret () =
            match instruction.operands with
            | [lone_operand] ->
                let (result, operand_interpreter) = read_operand interpreter lone_operand in
                handle_return operand_interpreter instruction result
            | _ -> failwith "instruction must have one operand" in

        let handle_rtrue () = handle_return interpreter instruction 1 in

        let handle_rfalse () = handle_return interpreter instruction 0 in

        let handle_call () =

            (* The packed address is already unpacked if the operand is a constant, but not if the operand is a variable. *)

            let routine_address_operand = List.hd instruction.operands in
            let routine_operands = List.tl instruction.operands in
            let (routine_address, routine_interpreter) =
                match routine_address_operand with
                | Large large -> (large, interpreter)
                | Small small -> (small, interpreter)
                | Variable Stack -> (decode_packed_address interpreter.story (peek_stack interpreter), pop_stack interpreter)
                | Variable Local local -> (decode_packed_address interpreter.story (IntMap.find local (current_frame interpreter).locals), interpreter)
                | Variable Global global -> (decode_packed_address interpreter.story (read_global interpreter.story global), interpreter) in

            (* We now have the routine address and its operands. Operands must be copied to locals.
               Locals must be given their default values first, and then if there are corresponding operands
               (the arguments are copied to the first n locals) then we overwrite them. *)

            let count = locals_count routine_interpreter.story routine_address in
            let rec create_default_locals map i =
                if i > count then map
                else create_default_locals (IntMap.add i (local_default_value routine_interpreter.story routine_address i) map) (i + 1) in
            let default_locals = create_default_locals IntMap.empty 1 in

            (* We now have a map that contains all the locals initialized to their default values. *)

            (* Now copy the arguments to the corresponding place in the locals map. *)
            (* Note that we must evaluate all the operands even if they are not being copied to locals; they might pop the stack. *)

            let rec copy_arguments operands_copied_interpreter remaining_operands acc_locals current_local =
                match remaining_operands with
                | [] -> (acc_locals, operands_copied_interpreter)
                | operand :: tail ->
                    let (argument_value, new_interpreter) = read_operand operands_copied_interpreter operand in
                    let new_locals = if current_local <= count then IntMap.add current_local argument_value acc_locals else acc_locals in
                    copy_arguments new_interpreter tail new_locals (current_local + 1) in

            let (locals, locals_interpreter) = copy_arguments routine_interpreter routine_operands default_locals 1 in

            (* We have evaluated all the operands; at this point we need to bail if the
               target address is zero. Calling zero is the same as calling a routine that
               does nothing but return false. *)

            if routine_address = 0 then
                handle_store_and_branch locals_interpreter instruction 0
            else
                let first_instruction = first_instruction locals_interpreter.story routine_address in
                let frame = { stack = []; locals = locals; called_from = instruction.address; called = first_instruction } in
                set_program_counter (add_frame interpreter frame) first_instruction in

        (* The big dispatch *)

        match instruction.opcode with
        | ILLEGAL -> failwith "illegal operand"
        | OP2_1   -> handle_je ()
        | OP2_2   -> handle_op2_value handle_jl
        | OP2_3   -> handle_op2_value handle_jg
        | OP2_4   -> handle_dec_chk interpreter instruction
        | OP2_5   -> handle_inc_chk interpreter instruction
        | OP2_6   -> handle_op2_value handle_jin
        | OP2_7   -> handle_op2_value handle_test
        | OP2_8   -> handle_op2_value handle_or
        | OP2_9   -> handle_op2_value handle_and
        | OP2_10  -> handle_op2_value handle_test_attr
        | OP2_11  -> handle_op2_effect handle_set_attr
        | OP2_12  -> handle_op2_effect handle_clear_attr
        | OP2_13  -> handle_store interpreter instruction
        | OP2_14  -> handle_op2_effect handle_insert_obj
        | OP2_15  -> handle_op2_value handle_loadw
        | OP2_16  -> handle_op2_value handle_loadb
        | OP2_17  -> handle_op2_value handle_get_prop
        | OP2_18  -> handle_op2_value handle_get_prop_addr
        | OP2_19  -> handle_op2_value handle_get_next_prop
        | OP2_20  -> handle_op2_value handle_add
        | OP2_21  -> handle_op2_value handle_sub
        | OP2_22  -> handle_op2_value handle_mul
        | OP2_23  -> handle_op2_value handle_div
        | OP2_24  -> handle_op2_value handle_mod
        | OP2_25
        | OP2_26
        | OP2_27
        | OP2_28 -> failwith "TODO: instruction for version greater than 3"

        | OP1_128 -> handle_op1_value handle_jz
        | OP1_129 -> handle_op1_value handle_get_sibling
        | OP1_130 -> handle_op1_value handle_get_child
        | OP1_131 -> handle_op1_value handle_get_parent
        | OP1_132 -> handle_op1_value handle_get_prop_len
        | OP1_133 -> handle_inc interpreter instruction
        | OP1_134 -> handle_dec interpreter instruction
        | OP1_135 -> handle_op1_effect handle_print_addr
        | OP1_136 -> failwith "TODO: instruction for version greater than 3"
        | OP1_137 -> handle_op1_effect handle_remove_obj
        | OP1_138 -> handle_op1_effect handle_print_obj
        | OP1_139 -> handle_ret ()
        | OP1_140 -> handle_jump ()
        | OP1_141 -> handle_op1_effect handle_print_paddr
        | OP1_142 -> handle_op1_value handle_load
        | OP1_143 -> handle_op1_value handle_not

        | OP0_176 -> handle_rtrue ()
        | OP0_177 -> handle_rfalse ()
        | OP0_178 -> handle_op0_effect handle_print
        | OP0_179 -> handle_print_ret ()
        | OP0_180 -> handle_op0_effect handle_nop
        | OP0_181 -> failwith "TODO: save"
        | OP0_182 -> failwith "TODO: restore"
        | OP0_183 -> handle_restart ()
        | OP0_184 -> handle_ret_popped ()
        | OP0_185 -> handle_op0_effect handle_pop
        | OP0_186 -> handle_quit ()
        | OP0_187 -> handle_op0_effect handle_new_line
        | OP0_188 -> handle_op0_effect handle_show_status
        | OP0_189 -> failwith "TODO: verify"
        | OP0_190 -> failwith "TODO: instruction for version greater than 3"
        | OP0_191 -> failwith "TODO: instruction for version greater than 3"

        | VAR_224 -> handle_call ()
        | VAR_225 -> handle_op3_effect handle_storew
        | VAR_226 -> handle_op3_effect handle_storeb
        | VAR_227 -> handle_op3_effect handle_putprop
        | VAR_228 -> handle_sread interpreter instruction
        | VAR_229 -> handle_op1_effect handle_print_char
        | VAR_230 -> handle_op1_effect handle_print_num
        | VAR_231 -> handle_op1 handle_random
        | VAR_232 -> handle_op1_effect handle_push
        | VAR_233 -> handle_pull interpreter instruction
        | VAR_234 -> failwith "TODO: split_window"
        | VAR_235 -> failwith "TODO: set_window"
        | VAR_236
        | VAR_237
        | VAR_238
        | VAR_239
        | VAR_240
        | VAR_241
        | VAR_242 -> failwith "TODO: instruction for version greater than 3"
        | VAR_243 -> handle_op1_effect handle_output_stream
        | VAR_244 -> failwith "TODO: input_stream"
        | VAR_245
        | VAR_246
        | VAR_247
        | VAR_248
        | VAR_249
        | VAR_250
        | VAR_251
        | VAR_252
        | VAR_253
        | VAR_254
        | VAR_255 -> failwith "TODO: instruction for version greater than 3";;

    let step interpreter =

        match interpreter.state with
        | Halted -> failwith "interpreter is halted"
        | Waiting_for_input -> failwith "interpreter is waiting for input"
        | Running -> ();

        let screen = if interpreter.screen.needs_more then
          { interpreter.screen with scroll_count = 0; needs_more = false }
        else
          interpreter.screen in

        if screen.needs_scroll then
            { interpreter with
                screen = scroll screen;
                has_new_output = true }
        else
            step_instruction { interpreter with screen = screen; has_new_output = false };;

    let display_locals interpreter =
        IntMap.fold (fun local value acc -> acc ^ (Printf.sprintf "local%01x=%04x " local value)) (current_frame interpreter).locals "";;

    let display_stack interpreter =
        List.fold_left (fun str x -> Printf.sprintf "%s %04x" str x ) "" (current_frame interpreter).stack;;

    let display_interpreter interpreter =
        let locals = display_locals interpreter in
        let stack = display_stack interpreter in
        let instr = display_instructions interpreter.story interpreter.program_counter 1 in
        locals ^ "\n" ^ stack ^ "\n" ^ instr;;

    let handle_input interpreter instruction key =
        let length = String.length interpreter.input in
        if key = "\r" then complete_sread { interpreter with input = "" } instruction interpreter.input
        else if key = "\b" then (
            if length = 0 then interpreter
            else { interpreter with input = (String.sub interpreter.input 0 (length - 1))})
        else if length >= interpreter.input_max then interpreter
        else { interpreter with input = interpreter.input ^ key };;

    let step_with_input interpreter key =
        let instruction = decode_instruction interpreter.story interpreter.program_counter in
        match instruction.opcode with
        | VAR_228 -> handle_input interpreter instruction (string_of_char key)
        | _ -> failwith "not waiting for input";;
end (* Interpreter *)

module Button = struct
  open Graphics;;

  type t =
  {
    x : int;
    y : int;
    width : int;
    height : int;
    text : string
  };;

  let make x y margin text =
    let (text_width, text_height) = text_size text in
    {
      x = x;
      y = y;
      width = text_width + 2 * margin;
      height = text_height + 2 * margin;
      text = text
    };;

  let draw button =
    let (text_width, text_height) = text_size button.text in
    set_color blue;
    fill_rect button.x button.y button.width button.height;
    moveto (button.x + (button.width - text_width) / 2) (button.y + (button.height - text_height) / 2);
    set_color white;
    draw_string button.text;
    set_color foreground;;

  let was_clicked button x y =
    (button.x <= x) &&
    (x <= (button.x + button.width)) &&
    (button.y <= y) &&
    (y <= button.y + button.height);;
end

module Debugger = struct

    open Graphics;;
    open Screen;;
    open Interpreter;;

    open_graph "";;
    auto_synchronize false;;
    set_font "Lucida Console";;

    let (text_width, text_height) = text_size "X";;

    type state =
      | Paused
      | Running
      | Halted
      | Stepping of int;;

    type action =
      | Pause
      | StepBackwards
      | StepForwards
      | Run
      | Quit
      | Keystroke of char
      | NoAction;;

    type t =
    {
      undo_stack : Interpreter.t list;
      redo_stack : Interpreter.t list;
      interpreter : Interpreter.t;
      state : state;
      keystrokes : string;
      buttons :  (Button.t * action) list;
    };;

    (* Extra line for status *)
    let screen_extent screen =
        (10, 10, screen.width * text_width, (screen.height + 1) * text_height);;

    let make interpreter =
      let (x, y, _, h) = screen_extent interpreter.screen in
      let margin = 20 in
      let gap = 10 in
      let button_y = y + h + gap in
      let button_list =
        [
          ("X", Quit);
          ("<|", StepBackwards);
          ("||", Pause);
          (">", Run);
          ("|>", StepForwards)
        ] in
        let button_map =
          let rec aux map buttons button_x =
            match buttons with
            | [] -> map
            | (caption, action) :: tail ->
              let new_button = Button.make button_x button_y margin caption in
              let new_x = button_x + new_button.Button.width + gap in
              aux ((new_button, action) :: map) tail new_x in
          aux [] button_list x in
      {
        undo_stack = [];
        redo_stack = [];
        interpreter = interpreter;
        state = Running;
        keystrokes = "";
        buttons = button_map
      };;

    let clear_screen screen =
        let (x, y, w, h) = screen_extent screen in
        set_color background;
        fill_rect x y w h;;

    let draw_status screen =
      let (x, y, _, _) = screen_extent screen in
      let status_color = blue in
      match screen.status with
      | None -> ()
      | Some status -> (
        set_color status_color;
        moveto x (y + text_height * screen.height);
        draw_string status);;

    let draw_string_at text x y =
      moveto x y;
      draw_string text;;

    let rec draw_screen screen =
        clear_screen screen;
        draw_status screen;
        set_color foreground;
        let (x, y, _, _) = screen_extent screen in
        let rec aux n =
          if n < screen.height then
            (draw_string_at (Deque.peek_front_at screen.lines n) x (y + text_height * n);
            aux (n + 1)) in
        aux 0 ;
        synchronize();;

    let trim_to_length text length =
      if (String.length text) <= length then text
      else String.sub text 0 length;;

      (* x and y in screen coordinates; width and height in characters *)
      let draw_before_current_after before current after x y width height =
        let before_color = blue in
        let current_color = black in
        let after_color = blue in
        let rec draw_before items n =
          if n < height then
            match items with
            | [] -> ()
            | text :: tail -> (
              draw_string_at text x (y + n * text_height);
              draw_before tail (n + 1)) in
        let rec draw_after items n =
          if n > 0 then
            match items with
            | [] -> ()
            | text :: tail -> (
              draw_string_at text x (y + n * text_height);
              draw_after tail (n - 1)) in
        set_color background;
        fill_rect x y (width * text_width) (height * text_height);
        set_color before_color;
        draw_before before (height / 2 + 1);
        set_color current_color;
        draw_string_at current x (y + text_height * (height / 2));
        set_color after_color;
        draw_after after (height / 2 - 1);
        set_color foreground;
        synchronize();;

    let draw_undo_redo debugger =
      let undo_color = blue in
      let redo_color = blue in
      let current_color = black in
      let interpreter = debugger.interpreter in
      let (screen_x, screen_y, screen_w, screen_h) = screen_extent interpreter.screen in
      let window_x = screen_x + screen_w + 10 in
      let window_y = screen_y in
      let instruction_width = 60 in
      let window_w = text_width * instruction_width in
      let window_h = text_height * interpreter.screen.height in
      let draw_line interp n =
        let text = trim_to_length (Story.display_instructions interp.story interp.program_counter 1) instruction_width in
        draw_string_at text window_x (window_y + text_height * n) in
      let rec draw_undo undo n =
        if n < interpreter.screen.height then
          match undo with
          | [] -> ()
          | h :: t -> (
            draw_line h n;
            draw_undo t (n + 1)) in
      let rec draw_redo redo n =
        if n > 0 then
          match redo with
          | [] -> ()
          | h :: t -> (
            draw_line h n;
            draw_redo t (n - 1)) in
      set_color background;
      fill_rect window_x window_y window_w window_h;
      set_color undo_color;
      draw_undo debugger.undo_stack (interpreter.screen.height / 2 + 1);
      set_color current_color;
      draw_line debugger.interpreter (interpreter.screen.height / 2);
      set_color redo_color;
      draw_redo debugger.redo_stack (interpreter.screen.height / 2 - 1);
      set_color foreground;
      synchronize();;

    let debugger_push_undo debugger new_interpreter =
      if new_interpreter.program_counter = debugger.interpreter.program_counter then
        { debugger with interpreter = new_interpreter; redo_stack = [] }
      else
        { debugger with interpreter = new_interpreter;
          undo_stack = debugger.interpreter :: debugger.undo_stack;
          redo_stack = [] };;

    let needs_more debugger =
      debugger.interpreter.screen.needs_more;;

    let has_keystrokes debugger =
      (String.length debugger.keystrokes) > 0;;

    let draw_interpreter debugger =
      let interpreter = debugger.interpreter in
      let screen = interpreter.screen in
      if interpreter.state = Waiting_for_input then
        draw_screen (fully_scroll (print screen interpreter.input))
      else if interpreter.has_new_output || (debugger.state = Paused) || (debugger.state = Halted) then
      (
        let screen_to_draw =
          if needs_more debugger then
            more screen
          else
            screen in
        draw_screen screen_to_draw
      );;

  let step_reverse debugger =
    match debugger.undo_stack with
    | [] -> debugger
    | h :: t -> { debugger with
      undo_stack = t;
      interpreter = h;
      redo_stack = debugger.interpreter :: debugger.redo_stack };;

  let step_forward debugger =
    match debugger.redo_stack with
    | h :: t -> { debugger with
      undo_stack = debugger.interpreter :: debugger.undo_stack;
      interpreter = h;
      redo_stack = t }
    | [] ->
      let interpreter = debugger.interpreter in
      match interpreter.state with
      | Interpreter.Waiting_for_input ->
        (* If we have pending keystrokes then take the first one off the queue
        and give it to the interpreter. Otherwise just put this on the undo
        stack and return to the caller otherwise unchanged. We can't progress
        until someone gives us a key. *)
        let (new_interpreter, new_keys) =
          if debugger.keystrokes = "" then
            (interpreter, debugger.keystrokes)
          else
            (step_with_input interpreter debugger.keystrokes.[0],
            String.sub debugger.keystrokes 1 ((String.length debugger.keystrokes) - 1)) in
        { (debugger_push_undo debugger new_interpreter) with keystrokes = new_keys }
      | Interpreter.Halted -> debugger (* TODO: Exception? *)
      | Interpreter.Running ->
        let new_interpreter = step interpreter in
        debugger_push_undo debugger new_interpreter;;

  let waiting_for_input debugger =
    match debugger.interpreter.state with
    | Waiting_for_input -> true
    | _ -> false;;

  let rec obtain_action debugger should_block =
    (* A keystroke observed with Poll is not removed from the queue
    of keystrokes! It will keep coming back every time we poll. We
    therefore only consider keystroke events as having happened
    when we are blocking while waiting for input. *)
    let events =
      if should_block then [Key_pressed; Button_down]
      else [Poll] in
    let status = wait_next_event events in
    if should_block && status.keypressed then
      Keystroke status.key
    else
      let action =
        let is_hit (button, _) =
          Button.was_clicked button status.mouse_x status.mouse_y in
        if status.button then
          match List.filter is_hit debugger.buttons with
          | [] -> NoAction
          | (_, action) :: _ -> action
        else
          NoAction in
      if action = NoAction && should_block then
        (* If we're blocking until something happens, do not report NoAction. *)
        obtain_action debugger should_block
      else
        action;;

  let pause debugger =
    { debugger with state = Paused };;

  let start_running debugger =
    { debugger with state = Running };;

  let clear_redo debugger =
    { debugger with redo_stack = [] };;

  let add_keystroke debugger key =
   { debugger with keystrokes = debugger.keystrokes ^ (string_of_char key) };;

  let remove_keystroke debugger =
    let k = debugger.keystrokes in
    { debugger with keystrokes = String.sub k 1 ((String.length k) - 1) };;

  let set_step_instruction debugger instruction =
    { debugger with state = Stepping instruction };;

  let maybe_step debugger =
    let should_step =
      match debugger.state with
      | Running -> true
      | Stepping instruction -> debugger.interpreter.program_counter = instruction
      | _ -> false in
    if should_step then step_forward debugger
    else debugger;;

  let halt debugger =
    { debugger with state = Halted };;

  let draw_routine_listing debugger =
    let current_instruction = debugger.interpreter.program_counter in
    let first_instruction = (current_frame debugger.interpreter).called in
    let story = debugger.interpreter.story in
    let current = Story.display_instruction (Story.decode_instruction story current_instruction) in
    let reachable = Story.all_reachable_addresses_in_routine story first_instruction in
    let sorted = List.sort compare reachable in
    let decode instr =
      (instr, Story.display_instruction (Story.decode_instruction story instr)) in
    let map = List.map decode sorted in
    let rec aux before after map =
      match map with
      | [] -> (before, after)
      | (addr, text) :: tail ->
        if addr < current_instruction then aux (text :: before) after tail
        else if addr > current_instruction then aux before (text :: after) tail
        else aux before after tail in
    let (before, after) = aux [] [] map in
    let screen = debugger.interpreter.screen in
    let (screen_x, screen_y, screen_w, screen_h) = screen_extent screen in
    draw_before_current_after before current (List.rev after) (screen_x + screen_w + 10) screen_y 60 screen.height;;


  (* TODO: Most of the methods in this module can be local to run *)

  let run debugger =

    let rec main_loop debugger =
       (* Put the debugger into the right state, depending on the interpreter *)
       let debugger = match (debugger.state, debugger.interpreter.state) with
       | (_, Interpreter.Halted) -> halt debugger
       | (Stepping instruction, _) -> if debugger.interpreter.program_counter = instruction then debugger else pause debugger
       | _ -> debugger in

      (* Under what circumstances do we need to block?
         1 if the debugger is not running then block
         2 if the debugger is running, has no queued input, and is waiting for input, then block
         3 if the debugger is running, has no queued input, and is waiting for --MORE--, then block
      *)
      let running =
        match debugger.state with
        | Halted
        | Paused -> false
        | _ -> true in

      let needs_more = needs_more debugger in
      let waiting_for_input = waiting_for_input debugger in
      let has_keystrokes = has_keystrokes debugger in
      let should_block = (not running) || ((not has_keystrokes) && (waiting_for_input || needs_more)) in
      draw_interpreter debugger;
      if should_block then draw_routine_listing debugger;
      let action = obtain_action debugger should_block in
      match action with
      | Pause -> main_loop (pause debugger)
      | StepBackwards -> main_loop (pause (step_reverse debugger))
      | StepForwards -> main_loop (set_step_instruction debugger debugger.interpreter.program_counter)
      | Run -> main_loop (start_running (clear_redo debugger))
      | Quit -> ()
      | Keystroke key -> main_loop (add_keystroke debugger key)
      | NoAction ->
        (* Suppose we blocked because we had --MORE-- but no queued keystrokes.
        If we then got here we must have queued up a keystroke, which is still
        in the queue. The step will clear out the needs_more, but we need to
        lose that keystroke *)
        let new_debugger =
          if needs_more && has_keystrokes then remove_keystroke debugger
          else debugger in
        main_loop (maybe_step new_debugger) in
      List.iter (fun (b, _) -> Button.draw b) debugger.buttons;
      main_loop debugger;;
end (* Debugger *)

open Debugger;;
let story = Story.load_story "ZORK1.DAT";;
let screen = Screen.make 50 80;;
let interpreter = Interpreter.make story screen;;
let debugger = Debugger.make interpreter;;
Debugger.run debugger;;
