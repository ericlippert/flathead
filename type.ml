module IntMap = Map.Make(struct type t = int let compare = compare end)
type bit_number = Bit_number of int
type bit_size = Bit_size of int
type byte_address = Byte_address of int
type word_address = Word_address of int
type abbreviation_number = Abbreviation of int
type abbrev_table_base = Abbreviation_table_base of int
type word_zstring_address = Word_zstring of int
type zstring_address = Zstring of int
type zchar = Zchar of int
type dictionary_table_base = Dictionary_table_base of int
type dictionary_base = Dictionary_base of int
type dictionary_address =  Dictionary_address of int
type dictionary_number = Dictionary of int
type word_separator_number = Word_separator_number of int
type object_base = Object_base of int
type property_defaults_table = Property_defaults_table of int
type object_tree_base = Object_tree_base of int
type object_number = Object of int
type object_address = Object_address of int
type property_header_address = Property_header of int
type attribute_number = Attribute of int
type attribute_address = Attribute_address of byte_address * bit_number
type local_variable = Local of int
type global_variable = Global of int
type instruction_address = Instruction of int
type packed_routine_address = Packed_routine of int
type packed_zstring_address = Packed_zstring of int
type routine_address = Routine of int
type global_table_base = Global_table_base of int


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
