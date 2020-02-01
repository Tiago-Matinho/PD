(* Retorna o próximo elemento duma lista *)
let next = function
	| [] 	-> 	0
	| h::t 	-> 	h;;

(* Retorna a lista recebida a partir do elemento atual *)
let next_list = function
	| [] 	-> 	[]
	| h::t 	-> 	t;;

(* Retorna o tamanho da lista recebido *)
let rec size_of = function
	| [] 	-> 	0
	| h::t 	-> 	1 + (size_of t);;


(* Retorna uma lista de tamanho n com o valor de x *)
let rec n_of_x_list n x =
	match n with
	| 0 	-> 	[]
	| _ 	-> 	x::(n_of_x_list (n-1) x);;


(* Retorna o valor das pistas adicionadas *)
let rec added_clues = function
	| [] 	-> 	0
	| h::t 	-> 	h + (added_clues t);;


(* Retorna o número de espaços necessários para completar a linha *)
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

(* Verifica se existe um número de x pistas seguidas numa lista l *)
let rec unified_boxes x l =
	if x = 0
	then (next l) = 0
	else
		match l with
		| [] 	-> 	false
		| h::t 	-> 	if h = 1
					then (unified_boxes (x - 1) t)
					else false;;

(* Verifica se uma linha condiz com as pistas dadas *)
(* Ver diagrama 1 *)
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


(* Verifica se todas as soluções se verificam perante as pistas *)
let rec check_collums clues solution =
	match clues with
	| [] 	->( match solution with
				| [] 	-> 	true
				| _ 	-> 	false)
	| ci::cf ->( match solution with
				| [] 	-> 	false
				| si::sf -> if (check_line ci si)
					then (check_collums cf sf)
					else false);; 


(*************************************************************************)

(* Retorna uma lista transposta da lista recebida *)
(* Sendo n onde começa a coluna *)
let rec transpose_aux n length solution =
	match solution with
	| [] -> []
	| h::t-> let jumped_first = (jump_n n solution) in
		let jumped_sec = (jump_n (length-n) jumped_first) in
			match jumped_first with
		 	| [] -> []
		 	| h2::_ -> h2::(transpose_aux n length jumped_sec);; 

(* Retorna a matriz transposta de uma lista *)
let rec transpose n length solution =
	if n == length then []
	else (transpose_aux n length solution)::(transpose (n+1) length solution);;


(*************************************************************************)


(* Retorna uma lista de listas das permutações para apenas uma pista *)
(* Ver diagrama 2 *)
let rec simple_permutation spaces clue m =
	match spaces with
	| 0 -> []
	| _ ->
		if clue > spaces
			then []
		else
			let new_spaces = spaces - clue in
			((n_of_x_list m 0) @ (n_of_x_list clue 1) @ (n_of_x_list new_spaces 0))
			:: (simple_permutation (spaces-1) clue (m+1));;


(* Retorna uma lista de listas de todas as permutações possíveis para uma linha *)
(* Ver diagrama 3 *)
let rec permutation clues size_left n =
	match clues with
	| [] -> [[]]
	| pi::[] -> (simple_permutation size_left pi 0)
	| pi::pf -> let new_size = size_left -(n+pi+1) and added = ((added_clues pf)+(added_spaces pf)) in
		if (new_size < added) then [[2]]
		else let new_sol = (n_of_x_list n 0) @ (n_of_x_list pi 1) @ [0] and
			next = (permutation pf new_size 0) in
			(add_on_tail new_sol next)@(permutation clues size_left (n+1));;


(* Verifica se foi adicionado um 2 a uma lista *)
let rec has_two = function
	| [] -> false
	| h::t -> if h=2 then true else (has_two t);;

(* Remove todas as permutações inválidas i.e. permutações com um 2*)
let rec remove_permutation = function
	| [] -> []
	| h::t -> if(has_two h) then remove_permutation t else h::(remove_permutation t);;

(* Chama as permutações para uma linha *)
let all_perms_aux clues size_left = (remove_permutation(permutation clues size_left 0));;


(* Retorna todas as permutações para todas as linhas *)
let rec all_perms clues width =
	match clues with
	| [] 	-> 	[]
	| h::t 	-> 	(all_perms_aux h width)::(all_perms t width);;


(*************************************************************************)

(* Retorna a premutação n duma lista de listas *)
let rec get_perm n = function
	| []	->	[]
	| h::t 	-> 	if n = 0
				then h
				else (get_perm (n - 1) t);;


(* Controí a permutação consoante a escolha *)
let rec build_perm choice solution =
	match choice with
	 | [] 	-> 	[]
	 | h::t -> 	match solution with
	 			| [] 	-> 	[]
	 			| si::sf->	(get_perm h si) @ (build_perm t sf);; 


(* Retorna uma lista com a ultima escolha possível *)
let rec all_choices solution =
	match solution with
	| [] 	-> 	[]
	| h::t 	-> 	let this = (size_of h) in
				this::(all_choices t);;


(* Serve de auxilio para criar as escolhas possíveis *)
let rec choices_aux max n = 
	if n > max then [] else [n]::(choices_aux max (n+1));; 


(* Constroí uma lista com todas as escolhas de permutações possíveis *)
let rec all_choices_in_list max n =
	match max with
	| [] 	-> 	[]
	| h::[] -> 	choices_aux h 0
	| h::t 	->	if n > h
				then []
				else let next = (all_choices_in_list t 0) and list_n = [n] in
					(add_on_tail list_n next) @ (all_choices_in_list max (n+1));;


(*************************************************************************)

(* Imprime um caracter consoante o valor *)
let print_aux n = (if n==0 then (print_string " .") else (print_string " X"));;
	
(* Imprime uma linha *)
let rec print_line length = function
	| [] -> print_string "\n"
	| h::t -> print_aux h ; (print_line length t);;

(* Imprime uma matriz *)
let rec print length solution =
	match solution with
	| [] -> print_string "\n\n"
	| h::t -> print_line length h ; print length t;;

(* Retorna uma lista até ao elemento n *)
let rec list_until n = function
	| [] -> []
	| h::t -> if n==0 then [] else h::(list_until (n-1) t);;

(* Constroi uma matriz recebendo uma lista *)
let rec build_matrix length solution = 
	match solution with
	| [] -> []
	| h::t -> (list_until length solution) :: (build_matrix length (jump_n length solution));;


(*************************************************************************)

(* Testa uma escolha *)
let test_perm collums choice perms width =
	let this = build_perm choice perms in
	let this_transpose = transpose 0 width this in
	check_collums collums this_transpose;;


(* Precorre todas as escolhas possíveis *)
let rec puzzle_aux collums choices perms width =
	match choices with
	| [] 	-> 	print_string "No solution\n"
	| h::t 	-> 	if test_perm collums h perms width
				then let correct = build_perm h perms in
					(print width (build_matrix width correct))
				else puzzle_aux collums t perms width;;

(* Recebe as pistas e funciona como main *)
let puzzle matriz =
	match matriz with
	| l::c::[] ->(
					let lines = l and collum = c in
					let width = (size_of collum) in
					let all_perms = (all_perms lines width) in
					let max_choices = all_choices all_perms in
					let all_possible_choices = all_choices_in_list max_choices 0 in
					(puzzle_aux collum all_possible_choices all_perms width)
				)
	| _ -> print_string "Invalid format!\n";;


(* Testes *)
let seta = puzzle [[[1];[3];[1;1;1];[1];[1]];[[1];[1];[5];[1];[1]]];;

let smile = puzzle [[[2;2];[2;2];[1];[1;1];[3]];[[2;1];[2;1];[1;1];[2;1];[2;1]]];;
