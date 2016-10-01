%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Matthias Moulin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(tile).

-export([start_tile/1, restart_tile/2]).

start_tile(Id)->
	debug:debug_tile("[Tile ~p]\t\t execute start_tile()\n", [Id]),
	tilelife(passive, Id, 0, false, #{}, 0, no_step(), no_check()).
	
restart_tile(Id, Value)->
	debug:debug_tile("[Tile ~p]\t\t execute restart_tile(~p)\n", [Id, Value]),
	tilelife(passive, Id, Value, false, #{}, 0, no_step(), no_check()).

no_step()->
	0.
no_check()->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tile lifecyle						  							  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
tilelife(State, Id, Value, Merged, Buffer, BufferSize, Step, Check)->
	receive
		{rc, NValue} ->
			debug:debug_tile("[Tile ~p]\t\t recieved {rc, ~p}\n", [Id, NValue]),
			get_manager() ! {ra, Id},
			debug:debug_tile("[Tile ~p]\t\t send {ra, ~p} to manager\n", [Id, Id]),
			tilelife(closed, Id, NValue, false, #{}, 0, no_step(), no_check()); 
		ro ->
			debug:debug_tile("[Tile ~p]\t\t recieved {ro}\n", [Id]),
			tilelife(passive, Id, Value, Merged, Buffer, BufferSize, Step, Check);
		new_turn ->
			case State of
				closed ->
					debug:debug_tile("[Tile ~p]\t\t recieved and rejected {new_turn}\n", [Id]),
					tilelife(State, Id, Value, Merged, Buffer, BufferSize, Step, Check);
				_ ->
					debug:debug_tile("[Tile ~p]\t\t recieved {new_turn}\n", [Id]),
					tilelife(passive, Id, Value, false, #{}, 0, no_step(), no_check())
			end;
		die ->
			debug:debug_tile("[Tile ~p]\t\t recieved {die}\n", [Id]),
			exit(killed);
		up ->
			case State of
				closed ->
					debug:debug_tile("[Tile ~p]\t\t recieved and rejected {up}\n", [Id]),
					tilelife(State, Id, Value, Merged, Buffer, BufferSize, Step, Check);
				_ ->
					debug:debug_tile("[Tile ~p]\t\t recieved {up}\n", [Id]),
					NStep = -4, NCheck = fun between_bounds/2,
					NBufferSize = handle_movement(Id, NStep, NCheck),
					tilelife(up, Id, Value, Merged, #{}, NBufferSize, NStep, NCheck)
			end;
		dn ->
			case State of
				closed ->
					debug:debug_tile("[Tile ~p]\t\t recieved and rejected {dn}\n", [Id]),
					tilelife(State, Id, Value, Merged, Buffer, BufferSize, Step, Check);
				_ ->
					debug:debug_tile("[Tile ~p]\t\t recieved {dn}\n", [Id]),
					NStep = 4, NCheck = fun between_bounds/2,
					NBufferSize = handle_movement(Id, NStep, NCheck),
					tilelife(dn, Id, Value, Merged, #{}, NBufferSize, NStep, NCheck)
			end;
		lx ->
			case State of
				closed ->
					debug:debug_tile("[Tile ~p]\t\t recieved and rejected {lx}\n", [Id]),
					tilelife(State, Id, Value, Merged, Buffer, BufferSize, Step, Check);
				_ ->
					debug:debug_tile("[Tile ~p]\t\t recieved {lx}\n", [Id]),
					NStep = -1, NCheck = fun same_row/2,
					NBufferSize = handle_movement(Id, NStep, NCheck),
					tilelife(lx, Id, Value, Merged, #{}, NBufferSize, NStep, NCheck)
			end;
		rx ->
			case State of
				closed ->
					debug:debug_tile("[Tile ~p]\t\t recieved and rejected {rx}\n", [Id]),
					tilelife(State, Id, Value, Merged, Buffer, BufferSize, Step, Check);
				_ ->
					debug:debug_tile("[Tile ~p]\t\t recieved {rx}\n", [Id]),
					NStep = 1, NCheck = fun same_row/2,
					NBufferSize = handle_movement(Id, NStep, NCheck),
					tilelife(rx, Id, Value, Merged, #{}, NBufferSize, NStep, NCheck)
			end;
		{yourValue, Repl} ->
			case State of
				closed ->
					debug:debug_tile("[Tile ~p]\t\t recieved and rejected {yourValue, ~p}\n", [Id, Repl]),
					tilelife(State, Id, Value, Merged, Buffer, BufferSize, Step, Check);
				_ ->
					debug:debug_tile("[Tile ~p]\t\t recieved {yourValue, ~p}\n", [Id, Repl]),
					send_tilevalue(Id, Value, Merged, Repl),
					tilelife(State, Id, Value, Merged, Buffer, BufferSize, Step, Check)
			end;
		{tilevalue, TileId, TileValue, TileMerged} ->
			case State of
				closed ->
					debug:debug_tile("[Tile ~p]\t\t recieved and rejected {tilevalue, ~p, ~p, ~p}\n", [Id, TileId, TileValue, TileMerged]),
					tilelife(State, Id, Value, Merged, Buffer, BufferSize, Step, Check);
				_ ->
					debug:debug_tile("[Tile ~p]\t\t recieved {tilevalue, ~p, ~p, ~p}\n", [Id, TileId, TileValue, TileMerged]),
					NBuffer = maps:put(TileId, {TileValue, TileMerged}, Buffer),
					case (maps:size(NBuffer) == BufferSize) of
						true->
							{NValue, LS} = handle_tilevalue(Id, Value, NBuffer, Step, Check),
							case (BufferSize == 3) of
								true ->
									send_finished(Id, LS);
								false -> 
									send_movement(Id, Id - Step, State)
							end,
							tilelife(passive, Id, NValue, Merged, #{}, 0, no_step(), no_check());
						false->
							tilelife(State, Id, Value, Merged, NBuffer, BufferSize, Step, Check)
					end
				end;
		{setvalue, NewValue, NewMerged} ->
			case State of
				closed ->
					debug:debug_tile("[Tile ~p]\t\t recieved and rejected {setvalue, ~p, ~p}\n", [Id, NewValue, NewMerged]),
					tilelife(State, Id, Value, Merged, Buffer, BufferSize, Step, Check);
				_ ->
					debug:debug_tile("[Tile ~p]\t\t recieved {setvalue, ~p, ~p}\n", [Id, NewValue, NewMerged]),
					tilelife(passive, Id, NewValue, NewMerged, #{}, 0, no_step(), no_check())
			end
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handling of movement 	(up, dn, lx, rx)						  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
between_bounds(Id1, Id2)-> 
	((1 =< Id1) and (Id1 =< 16)) and ((1 =< Id2) and (Id2 =< 16)).
same_row(Id1, Id2)-> 
	between_bounds(Id1, Id2) and (((Id1 - 1) div 4) == ((Id2 - 1) div 4)).

handle_movement(Id, Step, Check)->
	handle_movement(Id, Id, Step, Check, 0).
handle_movement(Id, Id1, Step, Check, Acc)->
	Id2 = Id1 + Step,
	case Check(Id1, Id2) of							
		true->	
			send_yourValue(Id, Id2),
			handle_movement(Id, Id2, Step, Check, Acc + 1);
		false->
			Acc	
	end.

send_yourValue(Id1, Id2)->	
	Repl = glob:regformat(Id1),
	try 
		glob:regformat(Id2) ! {yourValue, Repl},
		debug:debug_tile("[Tile ~p]\t\t send {yourValue, ~p} to ~p\n", [Id1, Repl, Id2])
	catch 
		_:Reason -> 
			debug:debug_tile("[Tile ~p]\t\t ~p in send {yourValue, ~p} to ~p\n", [Id1, Reason, Repl, Id2]),
			send_yourValue(Id1, Id2)
	end.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handling of value request (yourValue)							  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
send_tilevalue(Id, Value, Merged, Repl)->	
	try
		Repl ! {tilevalue, Id, Value, Merged},
		debug:debug_tile("[Tile ~p]\t\t send {tilevalue, ~p, ~p, ~p} to ~p\n", [Id, Id, Value, Merged, Repl])
	catch 
		_:Reason -> 
			debug:debug_tile("[Tile ~p]\t\t ~p in send {tilevalue, ~p, ~p, ~p} to ~p\n", [Id, Reason, Id, Value, Merged, Repl]),
			send_tilevalue(Id, Value, Merged, Repl)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handling of value receive (tilevalue)							  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
handle_tilevalue(Id, Value, Buffer, Step, Check)->
	I1 = Id + Step,
	{V1, M1} = maps:get(I1, Buffer, "Default value"),
	case (Value /= 0) of
		true ->
			case (V1 /= 0) of
				true -> 
					case ((Value == V1) and (M1 == false)) of
						true ->
							LS = get_lineshot(Id, 0, I1, Value + V1, Buffer),
							send_setvalue(Id, I1, Value + V1, true),
							{0, LS};
						false ->
							LS = get_lineshot(Id, Value, I1, V1, Buffer),
							{Value, LS}
					end;
				false ->
					empty_space(Id, Value, I1, Buffer, Step, Check)	
			end;
		false ->
			LS = get_lineshot(Id, Value, I1, V1, Buffer),
			{Value, LS}
	end.

empty_space(Id, Value, I, Buffer, Step, Check)->
	I2 = I + Step,
	case (Check(I, I2)) of
		true ->
			{V2, M2} = maps:get(I2, Buffer, "Default value"),
			case (V2 /= 0) of
				true -> 
					case ((Value == V2) and (M2 == false)) of
						true ->
							LS = get_lineshot(Id, 0, I2, Value + V2, Buffer),
							send_setvalue(Id, I2, Value + V2, true),
							{0, LS};
						false ->
							LS = get_lineshot(Id, 0, I, Value, Buffer),
							send_setvalue(Id, I, Value, false),
							{0, LS}
					end;
				false->
					empty_space(Id, Value, I2, Buffer, Step, Check)
			end;
		false ->
			case (Value /= 0) of
				true ->
					LS = get_lineshot(Id, 0, I, Value, Buffer),
					send_setvalue(Id, I, Value, false),
					{0, LS};
				false ->
					{V, _} = maps:get(I, Buffer, "Default value"),
					LS = get_lineshot(Id, Value, I, V, Buffer),
					{Value, LS}
			end
	end.
	
get_lineshot(Id1, V1, Id2, V2, Buffer)->
	Lineshot = [{Id1, V1}, {Id2, V2}],
	NBuffer = maps:remove(Id2, Buffer),
	maps:fold(fun(I, {V, _}, Acc) -> lists:append(Acc, [{I, V}]) end, Lineshot, NBuffer).

send_setvalue(Id1, Id2, Value, Merged)->	
	try
		glob:regformat(Id2) ! {setvalue, Value, Merged},
		debug:debug_tile("[Tile ~p]\t\t send {setvalue, ~p, ~p, ~p} to ~p\n", [Id1, Id2, Value, Merged, Id2])
	catch 
		_:Reason -> 
			debug:debug_tile("[Tile ~p]\t\t ~p in send {setvalue, ~p, ~p, ~p} to ~p\n", [Id1, Reason, Id2, Value, Merged, Id2]),
			send_setvalue(Id1, Id2, Value, Merged)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handling of next							  					  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
send_movement(Id1, Id2, State)->	
	try
		glob:regformat(Id2) ! State,
		debug:debug_tile("[Tile ~p]\t\t send {~p} to ~p\n", [Id1, State, Id2])
	catch 
		_:Reason -> 
			debug:debug_tile("[Tile ~p]\t\t ~p in send {~p} to ~p\n", [Id1, Reason, State, Id2]),
			send_movement(Id1, Id2, State)
	end.
	
send_finished(Id, LS)->	
	try
		get_manager() ! {finished, Id, LS},
		debug:debug_tile("[Tile ~p]\t\t send {finished, ~p, ~p} to manager\n", [Id, Id, LS])
	catch 
		_:Reason -> 
			debug:debug_tile("[Tile ~p]\t\t ~p in send {finished, ~p, ~p} to manager\n", [Id, Reason, Id, LS]),
			send_finished(Id, LS)
	end.
	
get_manager()->
	manager.
