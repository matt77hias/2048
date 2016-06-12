%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Matthias Moulin
% R0255811
% 2CW-MMC
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(debug).

-export([debug/2, debug_manager/2, debug_monitor/2, debug_tile/2, simp/0]).

debug(What, Arg) ->
	case is_debug() of
		true ->	log(What, Arg);
		false -> ok
	end.

debug_manager(What, Arg) ->
	case is_manager_debug() of
		true ->	log(What, Arg);
		false -> ok
	end.
	
debug_monitor(What, Arg) ->
	case is_monitor_debug() of
		true ->	log(What, Arg);
		false -> ok
	end.
	
debug_tile(What, Arg) ->
	case is_tile_debug() of
		true ->	log(What, Arg);
		false -> ok
	end.

% change this to enable debugging
is_debug() -> true.
is_manager_debug() -> true.
is_monitor_debug() -> true.
is_tile_debug() -> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logging					  							          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

log(What, Arg)->
	Dest = string:concat("./log/", "Log.txt"),
	log_to(Dest, What, Arg).			
	
log_to(Dest, What, Arg)->
	%io:format(What, Arg),
	{ok, F} = file:open(Dest, [write,append]),		
	io:format(F, What, Arg),
	file:close(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some test functions					  					      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

simp() ->
	register(glob:regformat(2), self()),
	receive
		X -> X
	end.