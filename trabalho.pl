checksubline(L, N, S) :-
	length(L, S),
	fd_domain(L, 0, 1),
	fd_exactly(N, L, 1),
	fd_labeling(L).