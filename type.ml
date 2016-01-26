type object_number = Object of int
type object_address = Object_address of int
type property_number = Property of int
type dictionary_number = Dictionary of int
type dictionary_address =  Dictionary_address of int
type attribute_number = Attribute of int
type attribute_address = Attribute_address of int
type routine_address = Routine of int
type packed_routine_address = Packed_routine of int
type packed_zstring_address = Packed_zstring of int
type local_variable = Local of int
type global_variable = Global of int
type instruction_address = Instruction of int
type abbreviation_number = Abbreviation of int
type zstring_address = Zstring of int
type variable_location =
  | Stack
  | Local_variable of local_variable
  | Global_variable of global_variable
