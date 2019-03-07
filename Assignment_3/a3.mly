%{
    open A1
%}

/* Tokens are defined below.  */
%token TRUE FALSE ABS PLUS MINUS MUL DIV MOD EXP LP RP NOT AND OR EQ GTA LTA GEQ LEQ IF THEN ELSE DEF DELIMITER EOF
%token <int> INT
%token <string> ID
%start main
%type <A1.exptree> main /* Return type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
Not immplemented if then else yet
*/

main:
	or_expression EOL   { $1 }
|	EOF { Done }
;

or_expression:
	and_expression OR or_expression { Disjunction($1, $3) }
|	and_expression { $1 }

and_expression:
		{ $1 }
|	term PLUS expression	{ Plus($1,$3) }
;

term:
	factor MUL term { Mult($1,$3) }
|	factor { $1 }
;

factor:
	num { $1 }
|	LP expression RP { InParen($2) } 
;

num:
	

