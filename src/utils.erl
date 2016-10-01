%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Matthias Moulin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(utils).

-export([new_list/2, equals_to_list/2, size_of_list/1, set_nth/3, get_nth/2, index_of/2, tup2list/1]).

new_list(0, _) -> [];
new_list(Size, Element) -> lists:append([Element], new_list(Size-1, Element)). 

equals_to_list([], []) -> true;
equals_to_list([H1|T1], [H2|T2]) ->
	case (H1 == H2) of
		true ->  equals_to_list(T1, T2);
		false -> false
	end.

size_of_list(List) -> size_of_list(List, 0).
size_of_list([], Acc) -> Acc;
size_of_list([_|Tail], Acc) -> size_of_list(Tail, Acc + 1).

set_nth(_, [], _) -> erlang:error(out_of_bounds);
set_nth(1, [_|Tail], New) -> [New|Tail];
set_nth(Index, [Head|Tail], New) -> [Head|set_nth(Index-1, Tail, New)].

get_nth(_, []) -> erlang:error(out_of_bounds);
get_nth(1, [Head|_]) -> Head;
get_nth(Index, [_|Tail]) -> get_nth(Index-1, Tail).	

index_of(Element, List) -> index_of(Element, List, 1).
index_of(_, [], _)  -> erlang:error(out_of_bounds);
index_of(Head, [Head|_], Index) -> Index;
index_of(Element, [_|Tail], Index) -> index_of(Element, Tail, Index+1).
	
tup2list(T) -> tup2list(T, size(T), []).
tup2list(_, 0, Acc) -> Acc;
tup2list(T, N, Acc) -> tup2list(T, N-1, [element(N,T)|Acc]).
