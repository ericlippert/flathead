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

type version =
  | V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8
