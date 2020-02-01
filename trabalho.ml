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

(*************************************************************************)

let rec build_collum n length solution =
	match solution with
	| [] -> []
	| h::t-> let jumped_first = (jump_n n solution) in
		let jumped_sec = (jump_n (length-n) jumped_first) in
			match jumped_first with
		 	| [] -> []
		 	| h2::_ -> h2::(build_collum n length jumped_sec);; 


let rec build_collum_all n length solution =
	if n == length then []
	else (build_collum n length solution)::(build_collum_all (n+1) length solution);;


let rec check_collums clues solution =
	match clues with
	| [] ->( match solution with
		| [] -> true
		| _ -> false)
	| ci::cf ->( match solution with
		| [] -> false
		| si::sf -> if (check_line ci si) then (check_collums cf sf) else false);; 


(*************************************************************************)


let rec n_of_x_list n x =
	match n with
	| 0 -> []
	| _ -> x::(n_of_x_list (n-1) x);;


let rec added_clues = function
	| [] -> 0
	| h::t -> h + (added_clues t);;


let rec added_spaces = function
	| [] -> 0
	| h::[] -> 0
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


let rec first_per block n a =
	if a > n then []
	else
	let calc = n-a in
	((n_of_x_list a 0) @ (n_of_x_list calc 1) @ block @ (n_of_x_list a 1) @ (n_of_x_list calc 0)) :: (first_per block n (a+1));; 


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


let full_perm clues size_left = (remove_permutation(permutation clues size_left 0));;


let rec all_perms clues size =
	match clues with
	| [] -> []
	| h::t -> (full_perm h size)::(all_perms t size);;


let rec get_n n = function
	| [] -> [2]
	| h::t -> (if n==0 then h else(get_n (n-1) t));;


(*************************************************************************)

let rec size_of = function
	| [] -> 0
	| h::t -> 1+(size_of t);;


let rec all_perms_line n solution =
	match solution with
	| [] -> []
	| h::[] -> h
	| h::t -> 	let size = (size_of h) in
				if n > size then []
				else
					let temp = (get_n n h) in
					let added = (add_on_tail temp (all_perms_line 0 t)) in
					added@(all_perms_line (n+1) solution);;


(*************************************************************************)


let print_aux n = (if n==0 then (print_string " .") else (print_string " X"));;
	

let rec print_line length = function
	| [] -> print_string "\n"
	| h::t -> print_aux h ; (print_line length t);;

(*
let rec print_new length solution =
	match solution with
	| [] -> print_string "\n"
	| h::t -> (print_line length 0 solution);; (print_new length (jump_n length solution));;
*)

let rec print length solution =
	match solution with
	| [] -> print_string "\nDone!\n"
	| h::t -> print_line length h ; print length t;;


let rec list_until n = function
	| [] -> []
	| h::t -> if n==0 then [] else h::(list_until (n-1) t);;


let rec build_matrix length solution = 
	match solution with
	| [] -> []
	| h::t -> (list_until length solution) :: (build_matrix length (jump_n length solution));;


(*************************************************************************)


let puzzle_aux clues line width =
	let transposed = (build_collum_all 0 width line) in
	(check_collums clues transposed);; 


let rec check_all_perms clues perms width =
	match perms with
	| [] -> print_string "No solution\n"
	| h::t ->( if (puzzle_aux clues h width) then (print width (build_matrix width h)) else (check_all_perms clues t width));;


let puzzle matriz =
	match matriz with
	| l::c::[] ->( let lines = l and collum = c in
		let height = (size_of lines) and width = (size_of collum) in
		let all_lines = (get_perms lines width) in
		(check_all_perms collum all_lines width));;


let get_perms clues length = (all_perms_line 0 (all_perms clues length));;


(*************************************************************************)


let seta = puzzle [[[1];[3];[1;1;1];[1];[1]];[[1];[1];[5];[1];[1]]];;


let test = puzzle [[[2;1];[2;2];[8];[8];[8];[3;2];[2;4];[3];[1;1];[2;1]]; [[6;2];[7;1];[5];[3];[3];[5];[4;1];[3;2];[3];[5]]];;