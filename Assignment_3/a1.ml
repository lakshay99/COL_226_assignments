(* Dummy implementation of A1 *)
open A0
exception Not_implemented
exception Invalid_exp
exception Invalid_Stack

type answer = Num of bigint | Bool of bool

type  exptree = Done
  | N of int (* Integer constant *)
  | B of bool (* Boolean constant *)
  | Var of string (* variable *)
  | Conjunction of exptree * exptree (* binary operators on booleans /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  | Equals of exptree * exptree      (* comparison operations on integers *)
  | GreaterTE of exptree * exptree   (* comparison operations on integers *)
  | LessTE of exptree * exptree      (* comparison operations on integers *)
  | GreaterT of exptree * exptree    (* comparison operations on integers *)
  | LessT of exptree * exptree       (* comparison operations on integers *)
  | InParen of exptree               (* expressions using parenthesis *)
  | IfThenElse of exptree * exptree * exptree (* a conditional expression *)
  | Tuple of int * exptree           (* creating n-tuples (n >= 0) *)
  | Project of int * exptree         (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Plus of exptree * exptree        (* binary operators on integers *)
  | Minus of exptree * exptree       (* binary operators on integers *)
  | Mult of exptree * exptree        (* binary operators on integers *)
  | Div of exptree * exptree         (* binary operators on integers *)
  | Rem of exptree * exptree         (* binary operators on integers *)
  | Nega of exptree       (* unary operators on booleans *)
  | Abs of exptree        (* unary operators on integers *)

type opcode = NCONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS
  | EQS | GTE | LTE | GT | LT | PAREN
  | BCONST of bool | CONJ | DISJ | NOT 
  | IFTE | TUPLE of int | PROJ of int
						
let rec stackmc_helper_1 stk tail func = (match stk with
								head_1::head_2::tail_1 -> stackmc ((func head_2 head_1)::tail_1) tail
							|	_ -> raise(Invalid_Stack))

and stackmc_helper_2 stk tail func = (match stk with
								head_1::tail_1 -> stackmc ((func head_1)::tail_1) tail
							|	_ -> raise(Invalid_Stack))
							
and stackmc stk pgm = match pgm with	
		[] -> (match stk with
				[] -> raise(Invalid_Stack)
			|	head::tail -> head)
	|	head::tail -> let stk_curried = (stackmc_helper_1 stk tail) in
					  let stk_curried_2 = (stackmc_helper_2 stk tail) in
					match head with 
						NCONST(i) -> stackmc (i::stk) tail
					|	PLUS -> (stk_curried add)
					|	TIMES -> (stk_curried mult)
					|	MINUS -> (stk_curried sub)
					|	DIV -> (stk_curried div)
					|	REM -> (stk_curried rem)
					|	ABS -> (stk_curried_2 abs)
					|	UNARYMINUS -> (stk_curried_2 minus)
					|	EQS -> (stk_curried eq)
					|	GTE -> (stk_curried geq)
					|	LTE -> (stk_curried leq)
					|	GT -> (stk_curried gt)
					|	LT -> (stk_curried lt)
					|	PAREN -> (stackmc stk tail) (* Doubtful *)
					|	BCONST(b) -> stackmc (b::stk) tail
					|	CONJ -> (match stk with
								head_1::head_2::tail_1 -> (stackmc ((head_1 && head_2)::tail_1) tail)
							|	_ -> raise(Invalid_Stack))
					|	DISJ -> (match stk with
								head_1::head_2::tail_1 -> (stackmc ((head_1 || head_2)::tail_1) tail)
							|	_ -> raise(Invalid_Stack))
					|	NOT -> (match stk with
								head_1::tail_1 -> (stackmc ((not(head_1))::tail_1) tail)
							|	_ -> raise(Invalid_Stack))
					|	IFTE -> (match stk with 
								head_1::head_2::head_3::tail_1 -> if head_1 then (stackmc (head_2::tail_1) tail) else (stackmc (head_3::tail_1) tail)   
							|	_ -> raise(Invalid_Stack))
					|	TUPLE(i) -> (* Dont know what to do here *)
					|	PROJ(i) -> (* Dont know what to do here *);;
					
let rec compile_helper code ex_1 ex_2 = (compile ex_1) @ (compile ex_2) @ [code]
								
and	compile ex = match ex with 
		N(i) -> [NCONST((mk_big i))]
	|	B(b) -> [BCONST(b)]
	|	Var(str) -> (* Dont know what to do here *)
	|	Conjunction(ex_1, ex_2) -> (compile_helper CONJ ex_1 ex_2)
	|	Disjunction(ex_1, ex_2) -> (compile_helper DISJ ex_1 ex_2)
	|	Equals(ex_1, ex_2) -> (compile_helper EQS ex_1 ex_2)
	|	GreaterTE(ex_1, ex_2) -> (compile_helper GTE ex_1 ex_2)
	|	LessTE(ex_1, ex_2) -> (compile_helper LTE ex_1 ex_2)
	|	GreaterT(ex_1, ex_2) -> (compile_helper GT ex_1 ex_2)
	|	LessT(ex_1, ex_2) -> (compile_helper LT ex_1 ex_2)
	|	InParen(ex_1) -> (compile ex_1) @ [PAREN]
	|	IfThenElse(ex_1, ex_2, ex_3) -> (compile ex_3) @ (compile ex_2) @ (compile ex_3) @ [IFTE]
	|	Tuple(i, ex_1) -> (* Dont know what to do here *)
	|	Project(i, ex_1) -> (* Dont know what to do here *)
	|	Plus(ex_1, ex_2) -> (compile_helper PLUS ex_1 ex_2)
	|	Minus(ex_1, ex_2) -> (compile_helper MINUS ex_1 ex_2)
	|	Mult(ex_1, ex_2) -> (compile_helper TIMES ex_1 ex_2)
	|	Div(ex_1, ex_2) -> (compile_helper DIV ex_1 ex_2)
	|	Rem(ex_1, ex_2) -> (compile_helper REM ex_1 ex_2)
	|	Nega(ex_1) -> (compile ex_1) @ [UNARYMINUS]
	|	Abs(ex_1) -> (compile ex_1) @ [ABS];;
	
let eval ex = match ex with
		N(i) -> Num(mk_big i)
	|	B(b) -> Bool(b)
	|	Var(var_name) -> 		(* Dont know what to do here *)
	|	Conjunction(ex_1, ex_2) -> let temp_1 = (eval ex_1) and temp_2 = (eval ex_2) in
									match temp_1 with
										Bool(bool_1) -> (match temp_2 with
														Bool(bool_2) -> Bool(bool_1 && bool_2)
													|	_ -> raise(Invalid_exp))
									|	_ -> raise(Invalid_exp)
	|	Disjunction(ex_1, ex_2) -> let temp_1 = (eval ex_1) and temp_2 = (eval ex_2) in
									match temp_1 with
										Bool(bool_1) -> (match temp_2 with
														Bool(bool_2) -> Bool(bool_1 || bool_2)
													|	_ -> raise(Invalid_exp))
									|	_ -> raise(Invalid_exp)
	|	Equals(ex_1, ex_2) -> let temp_1 = (eval ex_1) and temp_2 = (eval ex_2) in
								match temp_1 with 
								Num(num_1) -> (match temp_2 with 
												Num(num_2) -> if (eq num_1 num_2) then Bool(true) else Bool(false)
											|	_ -> raise(Invalid_exp))
								|	_ -> raise(Invalid_exp)
	|	GreaterTE(ex_1, ex_2) -> let temp_1 = (eval ex_1) and temp_2 = (eval ex_2) in
								match temp_1 with 
								Num(num_1) -> (match temp_2 with 
												Num(num_2) -> if (geq num_1 num_2) then Bool(true) else Bool(false)
											|	_ -> raise(Invalid_exp))
								|	_ -> raise(Invalid_exp)
	|	LessTE(ex_1, ex_2) -> let temp_1 = (eval ex_1) and temp_2 = (eval ex_2) in
								match temp_1 with 
								Num(num_1) -> (match temp_2 with 
												Num(num_2) -> if (leq num_1 num_2) then Bool(true) else Bool(false)
											|	_ -> raise(Invalid_exp))
								|	_ -> raise(Invalid_exp)
	|	GreaterT(ex_1, ex_2) -> let temp_1 = (eval ex_1) and temp_2 = (eval ex_2) in
								match temp_1 with 
								Num(num_1) -> (match temp_2 with 
												Num(num_2) -> if (gt num_1 num_2) then Bool(true) else Bool(false)
											|	_ -> raise(Invalid_exp))
								|	_ -> raise(Invalid_exp)
	|	LessT(ex_1, ex_2) -> let temp_1 = (eval ex_1) and temp_2 = (eval ex_2) in
								match temp_1 with 
								Num(num_1) -> (match temp_2 with 
												Num(num_2) -> if (lt num_1 num_2) then Bool(true) else Bool(false)
											|	_ -> raise(Invalid_exp))
								|	_ -> raise(Invalid_exp)
	|	InParen(ex_1) -> (eval ex_1)
	|	IfThenElse(ex_1, ex_2, ex_3) -> let temp = (eval ex_1) in
										match temp with
											Bool(bool_exp) -> if bool_exp then (eval ex_2) else (eval ex_3)
										|	_ -> raise(Invalid_exp)
	|	Tuple() -> (* Dont know what to do here *)
	|	Project() -> (* Dont know what to do here *)
	|	Plus(ex_1, ex_2) -> let temp_1 = (eval ex_1) and temp_2 = (eval ex_2) in
							match temp_1 with
								Num(num_1) -> (match temp_2 with
												Num(num_2) -> Num (add num_1 num_2)
											|	_ -> raise(Invalid_exp))
							|	_ -> raise(Invalid_exp)
	|	Minus(ex_1, ex_2) -> let temp_1 = (eval ex_1) and temp_2 = (eval ex_2) in
							match temp_1 with
								Num(num_1) -> (match temp_2 with
												Num(num_2) -> Num (sub num_1 num_2)
											|	_ -> raise(Invalid_exp))
							|	_ -> raise(Invalid_exp)
	|	Mult(ex_1, ex_2) -> let temp_1 = (eval ex_1) and temp_2 = (eval ex_2) in
							match temp_1 with
								Num(num_1) -> (match temp_2 with
												Num(num_2) -> Num (mult num_1 num_2)
											|	_ -> raise(Invalid_exp))
							|	_ -> raise(Invalid_exp)
	|	Div(ex_1, ex_2) -> let temp_1 = (eval ex_1) and temp_2 = (eval ex_2) in
							match temp_1 with
								Num(num_1) -> (match temp_2 with
												Num(num_2) -> Num (div num_1 num_2)
											|	_ -> raise(Invalid_exp))
							|	_ -> raise(Invalid_exp)
	|	Rem(ex_1, ex_2) -> let temp_1 = (eval ex_1) and temp_2 = (eval ex_2) in
							match temp_1 with
								Num(num_1) -> (match temp_2 with
												Num(num_2) -> Num (rem num_1 num_2)
											|	_ -> raise(Invalid_exp))
							|	_ -> raise(Invalid_exp)
	|	Nega(ex_1) ->  let temp_1 = (eval ex_1) in
							match temp_1 with
								Bool(b) -> Bool(not(b))
							|	_ -> raise(Invalid_exp)
	|	Abs(ex_1) -> let temp_1 = (eval ex_1) and temp_2 = (eval ex_2) in
							match temp_1 with
								Num(num_1) -> (match temp_2 with
												Num(num_2) -> Num (abs num_1 num_2)
											|	_ -> raise(Invalid_exp))
							|	_ -> raise(Invalid_exp);;