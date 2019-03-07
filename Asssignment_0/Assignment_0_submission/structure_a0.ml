open Signature_a0
	module A0 : BigInt = struct
	(* Your code goes here *)
	exception DivisionByZero

	(* Take first min(n, length(l)) elements of l *)
	(* Assumed zero is always NonNeg *)
	(* In division, assumed that remainder and divisor always have same sign and calculated quotient accordingly*)
	type bigint = sign * int list
		and sign = Neg | NonNeg

	let rec take n l =  match l with
							[ ] -> [ ]
						|   x::xs -> if n = 0 then [ ]
									else x::(take (n-1) xs);;

	let rec drop n l =  match l with
                        [ ] -> [ ]
                    |   x::xs -> if n = 0 then l
                        else (drop (n-1) xs);;

	let rec zeros n = if n = 0 then [] else 0::zeros(n-1);;


	let rec make_absolute_big abs_val =
		if abs_val > 0 then
			make_absolute_big (abs_val/10) @ [(abs_val mod 10)]
		else [ ];;

	let rec cmp_rev_absolute abs_val_1 abs_val_2 = match abs_val_1 with	(* assumes abs_val_1 and abs_val_2 are in reverse order and 1 num1>num2, 0 if num1=num2, -1 if num1 < num2 *)
			[]	->	(match abs_val_2 with
						[] -> 0
					|	head_2::tail_2 -> if head_2 = 0 then cmp_rev_absolute abs_val_1 tail_2 else -1)
		|	head_1::tail_1 ->( match abs_val_2 with
								[] -> if head_1 = 0 then cmp_rev_absolute tail_1 abs_val_2 else 1
							|	head_2::tail_2 -> let temp = cmp_rev_absolute tail_1 tail_2 in
													if (temp = 1 || temp = -1) then temp
													else if head_1 > head_2 then 1
														else if head_1 = head_2 then 0
															else -1);;

	let rec remove_redundant_zeroes temp = match temp with (* Takes the list in normal order *)
			[]	->	[]
		|	head::tail	->	if head = 0 then (remove_redundant_zeroes tail) else temp;;

	let rec print_abs_val abs_val =
		 match abs_val with
		head::tail -> (string_of_int head) ^ (print_abs_val tail)
	|	[] -> "" ;;

	let rec remove_redundant_zeroes_rev temp = match temp with (* Takes the list in reverse order and removes the redundant end zeroes*)
			[]	->	[]
		|	head::tail	->	List.rev (remove_redundant_zeroes (List.rev temp))

	let rec eq_rev_absolute abs_val_1 abs_val_2 = if ((cmp_rev_absolute abs_val_1 abs_val_2) = 0) then true else false;;

	let rec gt_rev_absolute abs_val_1 abs_val_2 = if ((cmp_rev_absolute abs_val_1 abs_val_2) = 1) then true else false;;

	let rec lt_rev_absolute abs_val_1 abs_val_2 = if ((cmp_rev_absolute abs_val_1 abs_val_2) = -1) then true else false;;

	let rec geq_rev_absolute abs_val_1 abs_val_2 = let temp = (cmp_rev_absolute abs_val_1 abs_val_2) in
							if ((temp = 0) || (temp = 1))  then true else false;;

	let rec leq_rev_absolute abs_val_1 abs_val_2 = let temp = (cmp_rev_absolute abs_val_1 abs_val_2) in
							if ((temp = 0) || (temp = -1)) then true else false;;

	let rec add_rev_num num1 num2 = match num1 with	(* num1 and num2 stored in reverse order *)
		[] -> remove_redundant_zeroes_rev num2
	|	head_1::tail_1 -> (match num2 with
							[] -> remove_redundant_zeroes_rev num1
						|	head_2::tail_2 -> remove_redundant_zeroes_rev (((head_1 + head_2) mod 10) :: add_rev_num (add_rev_num tail_1 tail_2) [(head_1 + head_2)/10]));;

	let rec sub_rev_num num1 num2 = match num1 with	(* num1 > num2 and stored in reverse order*)
		[] -> []
	|	head_1::tail_1 -> (match num2 with
							[] -> remove_redundant_zeroes_rev num1
						|	head_2::tail_2 -> if head_1 >= head_2 then remove_redundant_zeroes_rev ((head_1 - head_2) :: sub_rev_num (sub_rev_num tail_1 tail_2) [0])
											else remove_redundant_zeroes_rev ((head_1 - head_2 + 10) :: sub_rev_num (sub_rev_num tail_1 tail_2) [1]));;

	let rec mul_rev_num num1 num2 = match num1 with (* num1 and num2 are stored in reverse order *)
		[] -> []
	|	[x] -> (match num2 with
				[] -> []
			|	head_2::tail_2 -> remove_redundant_zeroes_rev (((x * head_2) mod 10) :: add_rev_num [((x * head_2)/10)] (mul_rev_num [x] tail_2)))
	|	head_1::tail_1 -> remove_redundant_zeroes_rev (add_rev_num (mul_rev_num [head_1] num2) (0 :: mul_rev_num tail_1 num2));;


	let rec div_num_helper num1 num2 temp = (* num1 and num2 are in normal order and quotient and remainder in normal order*)
		let num1_rev = List.rev num1 in
		let num2_rev = List.rev num2 in
		if (eq_rev_absolute num1_rev []) then ([0],[0])
		else
			if (lt_rev_absolute num1_rev (mul_rev_num num2_rev [temp])) then (div_num_helper num1 num2 (temp-1))
			else ([temp], List.rev (remove_redundant_zeroes_rev (sub_rev_num num1_rev (mul_rev_num num2_rev [temp]))));;

	let rec div_num num1 num2 num_1_take_digits =  (* num1 and num2 in normal order and computing num1/num2 *)
		if (eq_rev_absolute num2 []) then raise(DivisionByZero)
		else
			let num1_modified = remove_redundant_zeroes num1 in
			let num2_modified = remove_redundant_zeroes num2 in
			(* let zeroes_removed_from_num1 = (List.length num1) - List.length(num1_modified) in *)
			(* let num1_modified_take_digits = max 0 (num_1_take_digits - zeroes_removed_from_num1) in *)
			if ((List.length num1_modified) <= (num_1_take_digits)) then (div_num_helper num1_modified num2_modified 9)
			else
				if (lt_rev_absolute (List.rev num1_modified) (List.rev num2_modified)) then ([], num1_modified)
				else match (div_num_helper (take num_1_take_digits num1_modified) num2_modified 9) with
					(quotient, remainder) ->if (eq_rev_absolute quotient []) then  let (x,y) = (div_num num1_modified num2_modified (num_1_take_digits+1)) in
					 																																(x,y)
											else
												let num_1_rem = (drop num_1_take_digits num1_modified) in
												match num_1_rem with
													[] -> (quotient, List.rev (sub_rev_num (List.rev num1_modified) (mul_rev_num (List.rev quotient) (List.rev num2_modified))))
												|	_  -> let (temp1, temp2) = (div_num (List.append remainder num_1_rem) num2_modified 0) in
															let new_quotient = List .rev (add_rev_num (List.rev (List.append quotient (zeros (List.length num_1_rem)))) (List.rev temp1)) in
															let new_remainder = List.rev (sub_rev_num (List.rev num1_modified) (mul_rev_num (List.rev new_quotient) (List.rev num2_modified))) in
															(new_quotient, new_remainder);;







	let add big_int_1 big_int_2 =
		let (sign_1, abs_val_1) = big_int_1 in
		let (sign_2, abs_val_2) = big_int_2 in
		let abs_val_1_rev = List.rev abs_val_1 in
		let abs_val_2_rev = List.rev abs_val_2 in
		if ((sign_1 = Neg) && (sign_2 = NonNeg)) then
			if (gt_rev_absolute abs_val_1_rev abs_val_2_rev) then (Neg, List.rev (sub_rev_num abs_val_1_rev abs_val_2_rev))
			else (NonNeg, List.rev (sub_rev_num abs_val_2_rev abs_val_1_rev))
		else if ((sign_2 = Neg) && (sign_1 = NonNeg)) then
				if (gt_rev_absolute abs_val_2_rev abs_val_1_rev) then (Neg, List.rev (sub_rev_num abs_val_2_rev abs_val_1_rev))
				else (NonNeg, List.rev (sub_rev_num abs_val_1_rev abs_val_2_rev))
			else (sign_1, List.rev (add_rev_num abs_val_1_rev abs_val_2_rev));;

	let mult big_int_1 big_int_2 =
		let (sign_1, abs_val_1) = big_int_1 in
		let (sign_2, abs_val_2) = big_int_2 in
		let abs_val_1_rev = List.rev abs_val_1 in
		let abs_val_2_rev = List.rev abs_val_2 in
		let abs_rev_product = mul_rev_num abs_val_1_rev abs_val_2_rev in
		let abs_product = (List.rev abs_rev_product) in
		if (eq_rev_absolute abs_product []) then (NonNeg, [])
		else
			if(((sign_1 = Neg)&&(sign_2 = NonNeg))||((sign_1 = NonNeg)&&(sign_2 = Neg))) then
					(Neg, abs_product)
			else
				(NonNeg, abs_product);;

	let minus big_int =
	let (sign, abs_val) = big_int in
	if (eq_rev_absolute abs_val []) then (NonNeg, [])
	else
		if sign = Neg then (NonNeg, abs_val)
		else (Neg, abs_val);;

	let sub big_int_1 big_int_2 = add big_int_1 (minus big_int_2);;

	let div big_int_1 big_int_2 =
		let (sign_1, abs_val_1) = big_int_1 in
		let (sign_2, abs_val_2) = big_int_2 in
		let (abs_quotient, abs_rem) = (div_num abs_val_1 abs_val_2 0) in
		if (eq_rev_absolute abs_quotient []) then (NonNeg, abs_quotient)
		else
			if (sign_1 = sign_2) then
				(NonNeg, abs_quotient)
			else (Neg, abs_quotient);;
			(* if ((sign_1 = Neg && sign_2 = NonNeg) || (sign_1 = NonNeg && sign_2 = Neg)) then *)
		 		(* if (eq_rev_absolute [] abs_rem) then (Neg, abs_quotient) *)
				(* else (sub (Neg, abs_quotient) (NonNeg, [1])) *)
			(* else *)
				(* (NonNeg, abs_quotient);; *)

	let rem big_int_1 big_int_2 =
		let (sign_1, abs_val_1) = big_int_1 in
		let (sign_2, abs_val_2) = big_int_2 in
		let (abs_quotient, abs_rem) = (div_num abs_val_1 abs_val_2 0) in
		if (eq_rev_absolute abs_rem []) then (NonNeg, abs_rem)
		else (sign_1, abs_rem);;

	let eq big_int_1 big_int_2 =
		let (sign_1, abs_val_1) = big_int_1 in
		let (sign_2, abs_val_2) = big_int_2 in
		if ((sign_1 = Neg)&&(sign_2 = NonNeg)) then false
		else
			if (sign_1 = NonNeg)&&(sign_2 = Neg) then false
			else eq_rev_absolute (List.rev abs_val_1) (List.rev abs_val_2);;



	let abs big_int =
		let (sign, abs_val) = big_int in
			(NonNeg, abs_val);;

	let gt big_int_1 big_int_2 =
		let (sign_1, abs_val_1) = big_int_1 in
		let (sign_2, abs_val_2) = big_int_2 in
		if ((eq_rev_absolute abs_val_1 [])&&(eq_rev_absolute abs_val_2 [])) then false
		else
		if ((sign_1 = NonNeg) && (sign_2 = Neg)) then true
		else
			if ((sign_2 = NonNeg) && (sign_1 = Neg)) then false
			else gt_rev_absolute (List.rev abs_val_1) (List.rev abs_val_2);;

	let lt big_int_1 big_int_2 =
		let (sign_1, abs_val_1) = big_int_1 in
		let (sign_2, abs_val_2) = big_int_2 in
		if ((eq_rev_absolute abs_val_1 [])&&(eq_rev_absolute abs_val_2 [])) then false
		else
		if ((sign_1 = Neg)&&(sign_2 = NonNeg)) then true
		else
			if ((sign_1 = NonNeg) && (sign_2 = Neg)) then false
			else lt_rev_absolute (List.rev abs_val_1) (List.rev abs_val_2);;

	let geq big_int_1 big_int_2 =
		(gt big_int_1 big_int_2) || (eq big_int_1 big_int_2);;

	let leq big_int_1 big_int_2 =
		(lt big_int_1 big_int_2) || (eq big_int_1 big_int_2);;

	let print_num big_int =
		let (sign, abs_val) = big_int in
		if (eq_rev_absolute abs_val []) then "0"
		else
		let temp = remove_redundant_zeroes abs_val in
		if sign = Neg then "-" ^ (print_abs_val temp)
		else (print_abs_val temp);;

	let mk_big number =
		if number < 0 then (Neg, make_absolute_big (-number))
		else (NonNeg, make_absolute_big number);;
end
