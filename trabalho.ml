let next = function
	| [] -> 0
	| h::t -> h;;


let next_list = function
	| [] -> []
	| h::t -> t;;


let rec all_remaining x = function
	| [] -> true
	| h::t -> if h=x then (all_remaining x t) else false;;


let rec jump_all_xs x = function
	| [] -> []
	| h::t -> if h=x then (jump_all_xs x t) else h::t;;


let rec jump_n n = function
	| [] -> []
	| h::t -> if n=0 then h::t else (jump_n (n-1) t);;


let rec unified_boxes x l =
	if x=0 then (next l)=0
	else match l with
	| [] -> false
	| h::t -> if h=1 then (unified_boxes (x-1) t) else false;;


let rec check_line clues solution =
	match clues with
	| [] ->( match solution with
		| [] -> true
		| si::sf -> (all_remaining 0 (si::sf))
	)
	| ci::cf ->( match solution with
		| [] -> false
		| si::sf -> if (all_remaining 0 (si::sf)) then false
			else
				let jumped_zeros = (jump_all_xs 0 (si::sf)) in
				if(unified_boxes ci jumped_zeros) then 
					let jumped_ones = (jump_all_xs 1 jumped_zeros) in
					if (next jumped_ones)=0 then
						(check_line cf (next_list jumped_ones))
					else false
				else false );;



let rec n_of_x_list n x =
	match n with
	| 0 -> []
	| _ -> x::(n_of_x_list (n-1) x);;


let rec added_clues = function
	| [] -> 0
	| h::t -> h + (added_clues t);;


let rec added_spaces = function
	| [] -> 0
	| h::t -> 1+(added_spaces t);;


let rec add_on_tail head tail =
	match tail with
	| [] -> []
	| h::t -> (head @ h) :: (add_on_tail head t);;


let rec simple_permutation spaces clue m =
	match spaces with
	| 0 -> []
	| _ ->
		if clue > spaces
			then []
		else
			let new_spaces = spaces - clue in
			((n_of_x_list m 0) @ (n_of_x_list clue 1) @ (n_of_x_list new_spaces 0)) :: (simple_permutation (spaces-1) clue (m+1));;

(*
let rec permutation spaces n m clues solution =
	match clues with
	| [] -> []
	| h::[] ->(
			if (m+h)>spaces
				then [solution]
			else
				let new_solution = (n_of_x_list m 0) @ (n_of_x_list h 1) @ (n_of_x_list (spaces-(m+h)) 0) in
				[solution :: new_solution] :: (permutation spaces n (m+1) clues solution)
			)

	| h::t ->(
			if (n+h)>spaces
				then [solution]
			else
				let new_spaces = spaces-(n+h+1)
				and new_solution = [(n_of_x_list n 0) @ (n_of_x_list h 1) @ (n_of_x_list 1 0)] in
				solution :: (permutation new_spaces (n+1) 0 t new_solution)
			)
;;

*)