type byte_address = Byte_address of int
type word_address = Word_address of int
type input_buffer = Input_buffer of int
type parse_buffer = Parse_buffer of int
type string_address = String_address of int
type sz_address = Sz_address of int
type word_prefixed = Word_prefixed_string of int
type byte_prefixed = Byte_prefixed_string of int
type bit_number = Bit_number of int
type object_base = Object_base of int
type property_defaults_table = Property_defaults_table of int
type object_tree_base = Object_tree_base of int
type object_number = Object of int
type object_address = Object_address of int
type property_number = Property of int
type property_header_address = Property_header of int
type property_address = Property_address of int
type property_data_address = Property_data of int
type attribute_number = Attribute of int
type attribute_address = Attribute_address of byte_address * bit_number
type global_table_base = Global_table_base of int
type dictionary_number = Dictionary of int
type dictionary_address =  Dictionary_address of int
type routine_address = Routine of int
type packed_routine_address = Packed_routine of int
type packed_zstring_address = Packed_zstring of int
type word_zstring_address = Word_zstring of int
type local_variable = Local of int
type global_variable = Global of int
type instruction_address = Instruction of int
type abbreviation_number = Abbreviation of int
type zstring_address = Zstring of int
type zchar = Zchar of int
type status_line = Status of string option
type character_width = Character_width of int
type character_height = Character_height of int
type character_x = Character_x of int
type character_y = Character_y of int
type pixel_width = Pixel_width of int
type pixel_height = Pixel_height of int
type pixel_x = Pixel_x of int
type pixel_y = Pixel_y of int
type cursor = Cursor of character_x * character_y
type checksum = Checksum of int
type colours_supported = Colours_supported of bool
type pictures_supported = Pictures_supported of bool
type boldface_supported = Boldface_supported of bool
type italics_supported = Italics_supported of bool
type fixed_pitch_supported = Fixed_pitch_supported of bool
type tandy_mode_enabled = Tandy_mode_enabled of bool
type screen_split_supported = Screen_split_supported of bool
type status_line_supported = Status_line_supported of bool
type sound_effects_supported = Sound_effects_supported of bool
type default_pitch = Default_is_variable_pitch of bool
type timed_keyboard_supported = Timed_keyboard_supported of bool
type transcript_enabled = Transcript_enabled of bool
type force_fixed_pitch = Force_fixed_pitch of bool
type draw_status_requested = Draw_status_requested of bool
type pictures_requested = Pictures_requested of bool
type undo_requested = Undo_requested of bool
type mouse_requested = Mouse_requested of bool
type colours_requested = Colours_requested of bool
type sound_requested = Sound_requested of bool
type menus_requested = Menus_requested of bool
type release_number = Release_number of int
type high_memory_base = High_memory_base of int
type static_memory_base = Static_memory_base of int
type dictionary_base = Dictionary_base of int
type dictionary_table_base = Dictionary_table_base of int
type serial_number = Serial_number of string
type abbrev_table_base = Abbreviation_table_base of int
type file_size = File_size of int
type interpreter_number = Interpreter_number of int
type interpreter_version = Interpreter_version of int
type colour_number = Colour of int
type terminating_characters_base = Terminating_characters_base of int
type revision = Revision of int * int
type alphabet_table = Alphabet_table of int
type compressed_bytes = Compressed of string
type uncompressed_bytes = Uncompressed of string
type word_separator_number = Word_separator_number of int


type version =
  | V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8

type variable_location =
  | Stack
  | Local_variable of local_variable
  | Global_variable of global_variable

type operand =
  | Large of int
  | Small of int
  | Variable of variable_location

type branch_address =
  | Return_true
  | Return_false
  | Branch_address of instruction_address

type status_line_kind =
  | NoStatus
  | ScoreStatus
  | TimeStatus

type window_selection =
  | Upper_window
  | Lower_window

(* TODO: Make these of bool *)
type scroll_enabled =
  | Scroll_enabled
  | Scroll_disabled

type wrap_enabled =
  | Word_wrap_enabled
  | Word_wrap_disabled

type more_enabled =
  | More_enabled
  | More_disabled

type scroll_pending =
  | Scroll_pending of string
  | Nothing_pending



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
