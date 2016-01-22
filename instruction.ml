open Utility

type opcode_form =
  | Long_form
  | Short_form
  | Variable_form
  | Extended_form

type operand_count =
  | OP0
  | OP1
  | OP2
  | VAR

type variable_location =
  | Stack
  | Local of int
  | Global of int

type operand_type =
  | Large_operand
  | Small_operand
  | Variable_operand
  | Omitted

type operand =
  | Large of int
  | Small of int
  | Variable of variable_location

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
  | EXT_0   | EXT_1   | EXT_2   | EXT_3   | EXT_4   | EXT_5   | EXT_6   | EXT_7
  | EXT_8   | EXT_9   | EXT_10  | EXT_11  | EXT_12  | EXT_13  | EXT_14
  | EXT_16  | EXT_17  | EXT_18  | EXT_19  | EXT_20  | EXT_21  | EXT_22  | EXT_23
  | EXT_24  | EXT_25  | EXT_26  | EXT_27  | EXT_28  | EXT_29
  | ILLEGAL

(* The tables which follow are maps from the opcode identification number
   to the opcode type; the exact order matters. *)

let one_operand_bytecodes = [|
  OP1_128; OP1_129; OP1_130; OP1_131; OP1_132; OP1_133; OP1_134; OP1_135;
  OP1_136; OP1_137; OP1_138; OP1_139; OP1_140; OP1_141; OP1_142; OP1_143  |]

let zero_operand_bytecodes = [|
  OP0_176; OP0_177; OP0_178; OP0_179; OP0_180; OP0_181; OP0_182; OP0_183;
  OP0_184; OP0_185; OP0_186; OP0_187; OP0_188; OP0_189; OP0_190; OP0_191  |]

let two_operand_bytecodes =[|
  ILLEGAL; OP2_1;  OP2_2;  OP2_3;  OP2_4;  OP2_5;   OP2_6;   OP2_7;
  OP2_8;   OP2_9;  OP2_10; OP2_11; OP2_12; OP2_13;  OP2_14;  OP2_15;
  OP2_16;  OP2_17; OP2_18; OP2_19; OP2_20; OP2_21;  OP2_22;  OP2_23;
  OP2_24;  OP2_25; OP2_26; OP2_27; OP2_28; ILLEGAL; ILLEGAL; ILLEGAL |]

let var_operand_bytecodes = [|
  VAR_224; VAR_225; VAR_226; VAR_227; VAR_228; VAR_229; VAR_230; VAR_231;
  VAR_232; VAR_233; VAR_234; VAR_235; VAR_236; VAR_237; VAR_238; VAR_239;
  VAR_240; VAR_241; VAR_242; VAR_243; VAR_244; VAR_245; VAR_246; VAR_247;
  VAR_248; VAR_249; VAR_250; VAR_251; VAR_252; VAR_253; VAR_254; VAR_255 |]

let ext_bytecodes = [|
  EXT_0;   EXT_1;   EXT_2;   EXT_3;   EXT_4;   EXT_5;   EXT_6;   EXT_7;
  EXT_8;   EXT_9;   EXT_10;  EXT_11;  EXT_12;  EXT_13;  EXT_14;  ILLEGAL;
  EXT_16;  EXT_17;  EXT_18;  EXT_19;  EXT_20;  EXT_21;  EXT_22;  EXT_23;
  EXT_24;  EXT_25;  EXT_26;  EXT_27;  EXT_28;  EXT_29;  ILLEGAL; ILLEGAL |]

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
}

let is_call ver opcode =
  match opcode with
  | OP1_143 (* call_1n in v5, logical not in v1-4 *)
    -> ver >= 5
  | VAR_224 (* call / call_vs *)
  | OP1_136 (* call_1s *)
  | OP2_26  (* call_2n *)
  | OP2_25  (* call_2s *)
  | VAR_249 (* call_vn *)
  | VAR_250 (* call_vn2 *)
  | VAR_236 (* call_vs2 *) -> true
  | _ -> false

let decode_variable n =
  if n = 0 then Stack
  else if n < 0x10 then Local n
  else Global n

let display_instruction instr ver =
  (* We match Inform's convention of numbering the locals and globals from zero *)
  let display_variable variable =
    match variable with
    | Stack -> "sp"
    | Local local -> Printf.sprintf "local%d" (local - 1)
    | Global global -> Printf.sprintf "g%02x" (global - 16) in

  let display_operands () =
    let to_string operand =
      match operand with
      | Large large -> Printf.sprintf "%04x " large
      | Small small -> Printf.sprintf "%02x " small
      | Variable variable -> (display_variable variable) ^ " " in
    accumulate_strings to_string instr.operands in

  let display_store () =
    match instr.store with
    | None -> ""
    | Some variable -> "->" ^ (display_variable variable) in

  let display_branch () =
    match instr.branch with
    | None -> ""
    | Some (true, Return_false) -> "?false"
    | Some (false, Return_false) -> "?~false"
    | Some (true, Return_true) -> "?true"
    | Some (false, Return_true) -> "?~true"
    | Some (true, Branch_address address) -> Printf.sprintf "?%04x" address
    | Some (false, Branch_address address) -> Printf.sprintf "?~%04x" address in

  let display_text () =
    match instr.text with
    | None -> ""
    | Some str -> str in

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
    | OP1_143 -> if ver <= 4 then "not" else "call_1n"
    | OP0_176 -> "rtrue"
    | OP0_177 -> "rfalse"
    | OP0_178 -> "print"
    | OP0_179 -> "print_ret"
    | OP0_180 -> "nop"
    | OP0_181 -> "save"
    | OP0_182 -> "restore"
    | OP0_183 -> "restart"
    | OP0_184 -> "ret_popped"
    | OP0_185 -> if ver <= 4 then "pop" else "catch"
    | OP0_186 -> "quit"
    | OP0_187 -> "new_line"
    | OP0_188 -> "show_status"
    | OP0_189 -> "verify"
    | OP0_190 -> "EXTENDED"
    | OP0_191 -> "piracy"
    | VAR_224 -> if ver <= 3 then "call" else "call_vs"
    | VAR_225 -> "storew"
    | VAR_226 -> "storeb"
    | VAR_227 -> "put_prop"
    | VAR_228 -> if ver <= 4 then "sread" else "aread"
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
    | VAR_255 -> "check_arg_count"
    | EXT_0   -> "save"
    | EXT_1   -> "restore"
    | EXT_2   -> "log_shift"
    | EXT_3   -> "art_shift"
    | EXT_4   -> "set_font"
    | EXT_5   -> "draw_picture"
    | EXT_6   -> "picture_data"
    | EXT_7   -> "erase_picture"
    | EXT_8   -> "set_margins"
    | EXT_9   -> "save_undo"
    | EXT_10  -> "restore_undo"
    | EXT_11  -> "print_unicode"
    | EXT_12  -> "check_unicode"
    | EXT_13  -> "set_true_colour"
    | EXT_14  -> "sound_data"
    | EXT_16  -> "move_window"
    | EXT_17  -> "window_size"
    | EXT_18  -> "window_style"
    | EXT_19  -> "get_wind_prop"
    | EXT_20  -> "scroll_window"
    | EXT_21  -> "pop_stack"
    | EXT_22  -> "read_mouse"
    | EXT_23  -> "mouse_window"
    | EXT_24  -> "push_stack"
    | EXT_25  -> "put_wind_prop"
    | EXT_26  -> "print_form"
    | EXT_27  -> "make_menu"
    | EXT_28  -> "picture_table"
    | EXT_29  -> "buffer_screen" in

  let start_addr = instr.address in
  let name = opcode_name instr.opcode in
  let operands = display_operands () in
  let store = display_store() in
  let branch = display_branch() in
  let text = display_text() in
  Printf.sprintf "%04x: %s %s%s %s %s\n"
    start_addr name operands store branch text
  (* End of display_instruction *)
