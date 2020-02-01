let next = function
	| [] 	-> 	0
	| h::t 	-> 	h;;


let next_list = function
	| [] 	-> 	[]
	| h::t 	-> 	t;;


let rec size_of = function
	| [] 	-> 	0
	| h::t 	-> 	1 + (size_of t);;


(* Returns a list of size n with x value *)
let rec n_of_x_list n x =
	match n with
	| 0 	-> 	[]
	| _ 	-> 	x::(n_of_x_list (n-1) x);;


(* Returns value of added clues *)
let rec added_clues = function
	| [] 	-> 	0
	| h::t 	-> 	h + (added_clues t);;


(* Returns number of spaces needed *)
let rec added_spaces = function
	| [] 	-> 	0
	| h::[] -> 	0
	| h::t 	-> 	1 + (added_spaces t);;


(* Recebe uma lista (head) que vai inserir à esquerda de todas as listas da tail *)
let rec add_on_tail head tail =
	match tail with
	| [] 	->	[]
	| h::t 	->	(head @ h) :: (add_on_tail head t);;


(* Verifica se todos os valores da lista são iguais a x *)
let rec all_remaining x = function
	| [] 	-> 	true
	| h::t 	-> 	if h = x
				then (all_remaining x t)
				else false;;


(* Retorna uma sublista em que se remove a parte da esquerda que contenha x *)
let rec jump_all_xs x = function
	| [] 	-> 	[]
	| h::t 	-> 	if h = x
				then (jump_all_xs x t)
				else h :: t;;


(* Retorna uma sublista a partir da posição n *)
let rec jump_n n = function
	| [] 	-> 	[]
	| h::t 	-> 	if n = 0
				then h :: t
				else (jump_n (n - 1) t);;


(* Retorna o valor de indice n duma lista *)
let rec get_n n = function
	| [] 	->	[2]
	| h::t 	-> 	if n = 0
				then h
				else (get_n (n - 1) t);;


(*************************************************************************)



(*************************************************************************)


let rec unified_boxes x l =
	if x = 0
	then (next l) = 0
	else
		match l with
		| [] 	-> 	false
		| h::t 	-> 	if h = 1
					then (unified_boxes (x - 1) t)
					else false;;


let rec check_line clues solution =
	match clues with
	| [] 	->(
				match solution with
				| [] 	-> 	true
				| si::sf-> 	(all_remaining 0 (si :: sf))
	)
	| ci::cf ->(
				match solution with
				| [] 	-> 	false
				| si::sf-> 	if all_remaining 0 (si :: sf)
							then false
							else
								let jumped_zeros = (jump_all_xs 0 (si :: sf)) in
								if(unified_boxes ci jumped_zeros)
								then 
									let jumped_ones = (jump_all_xs 1 jumped_zeros) in
									if (next jumped_ones) = 0
									then
									(check_line cf (next_list jumped_ones))
									else false
								else false
				);;


let rec check_collums clues solution =
	match clues with
	| [] ->( match solution with
		| [] -> true
		| _ -> false)
	| ci::cf ->( match solution with
		| [] -> false
		| si::sf -> if (check_line ci si) then (check_collums cf sf) else false);; 


(*************************************************************************)

let rec transpose_aux n length solution =
	match solution with
	| [] -> []
	| h::t-> let jumped_first = (jump_n n solution) in
		let jumped_sec = (jump_n (length-n) jumped_first) in
			match jumped_first with
		 	| [] -> []
		 	| h2::_ -> h2::(transpose_aux n length jumped_sec);; 


let rec transpose n length solution =
	if n == length then []
	else (transpose_aux n length solution)::(transpose (n+1) length solution);;


(*************************************************************************)


let rec simple_permutation spaces clue m =
	match spaces with
	| 0 -> []
	| _ ->
		if clue > spaces
			then []
		else
			let new_spaces = spaces - clue in
			((n_of_x_list m 0) @ (n_of_x_list clue 1) @ (n_of_x_list new_spaces 0)) :: (simple_permutation (spaces-1) clue (m+1));;


let rec permutation clues size_left n =
	match clues with
	| [] -> [[]]
	| pi::[] -> (simple_permutation size_left pi 0)
	| pi::pf -> let new_size = size_left -(n+pi+1) and added = ((added_clues pf)+(added_spaces pf)) in
		if (new_size < added) then [[2]]
		else let new_sol = (n_of_x_list n 0) @ (n_of_x_list pi 1) @ [0] and
			next = (permutation pf new_size 0) in
			(add_on_tail new_sol next)@(permutation clues size_left (n+1));;


let rec has_two = function
	| [] -> false
	| h::t -> if h=2 then true else (has_two t);;


let rec remove_permutation = function
	| [] -> []
	| h::t -> if(has_two h) then remove_permutation t else h::(remove_permutation t);;


let all_perms_aux clues size_left = (remove_permutation(permutation clues size_left 0));;


let rec all_perms clues width =
	match clues with
	| [] 	-> 	[]
	| h::t 	-> 	(all_perms_aux h width)::(all_perms t width);;


(*************************************************************************)


let rec get_perm n = function
	| []	->	[]
	| h::t 	-> 	if n = 0
				then h
				else (get_perm (n - 1) t);;


let rec build_perm choice solution =
	match choice with
	 | [] 	-> 	[]
	 | h::t -> 	match solution with
	 			| [] 	-> 	[]
	 			| si::sf->	(get_perm h si) @ (build_perm t sf);; 


let rec all_choices solution =
	match solution with
	| [] 	-> 	[]
	| h::t 	-> 	let this = (size_of h) in
				this::(all_choices t);;


let rec choices_aux max n = 
	if n > max then [] else [n]::(choices_aux max (n+1));; 


let rec change_one pos value max =
	let check = get_n pos max in
	if value > check then []
	else let pre = (n_of_x_list (pos - 1) 0) in
		let test = [value] in
		pre@test@(choices_aux(jump_n pos max));;

(*************************************************************************)

let lines = [[4;1];[4;3];[5;2];[5;4];[3;4];[3];[2;3];[2];[3];[2]];;

let collum = [[5;3];[5;4];[5;2];[4];[2];[2];[4];[6];[4];[4]];;

let width = size_of collum;;

let all_perms = (all_perms lines width);;

let max_choices = all_choices all_perms;;

let first = change_one 2 1 max_choices;;
