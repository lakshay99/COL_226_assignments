{
  open A3
  exception Not_implemented
}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file
*)

let  whitespace = [' ' '\t']
let digit = ['0'-'9']
let digits = digit
let integer = (['-']? (whitespace)? ['1'-'9'] digits*)|("-0")|("0")
let uppercase_letters = ['A' - 'Z']
let lowercase_letters = ['a' - 'z']
let letters = uppercase_letters| lowercase_letters
let id = lowercase_letters (letters|digit)*
let bool_true =  "T"
let bool_false =  "F"
let abs_func =  "abs"
let add_func =  "+"
let sub_func =   "-"
let mul_func =   "*"
let div_func =   "div"
let mod_func =   "mod"
let exp_func =   "^"
let left_parenthesis =   "("
let right_parenthesis =   ")"
let not_func =   "not"
let and_func =   "/\\"
let or_func =   "\\/"
let equal_to =   "="
let greater_than =   ">"
let less_than =   "<"
let greater_than_equal_to =   ">="
let less_than_equal_to =   "<="
let if_cond =   "if"
let then_cond =   "then"
let else_cond =   "else"
let def_construct =   "def"
let delimiter = ";"
let empty_str = ""

rule read = parse
	eof                { EOF }
	| 
	| integer as n           {(INT (int_of_string n))}
	| bool_true 		 { TRUE }
	| bool_false		 { FALSE }
	| abs_func		 {(ABS }
	| add_func		 {(PLUS)}
	| sub_func		 {(MINUS)}
	| mul_func		 {(MUL)}
	| div_func		 {(DIV)}
	| mod_func		 {(MOD)}
	| exp_func		 {(EXP)}
	| left_parenthesis	 {(LP)}
	| right_parenthesis	 {(RP)}
	| not_func		 {(NOT)}
	| and_func 		 {(AND)}
	| or_func		 {(OR)}
	| equal_to		 {(EQ)}
	| greater_than		 {(GTA)}
	| less_than		 {(LTA)}
	| greater_than_equal_to  {(GEQ)}
	| less_than_equal_to	 {(LEQ)}
	| if_cond		 {(IF)}
	| then_cond		 {(THEN)}
	| else_cond		 {(ELSE)}
	| id as s                {(ID (s))}
	| def_construct		 {(DEF)}
	| delimiter		 {(DELIMITER)}
	| _                { raise Not_implemented }
