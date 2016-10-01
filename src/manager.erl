%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Matthias Moulin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(manager).

-export([manage/0]).

manage() ->
	debug:debug_manager("[Manager]\t\t execute manage()\n", []),
	TileMap = start_tiles(),
	self() ! {collectedData, false},
	Snapshot = get_init_snapshot(),
	manageloop(passive, Snapshot, Snapshot, #{}, TileMap, get_init_rollback()).

get_tile_ids() ->
	[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16].
	
get_init_snapshot() ->
	utils:new_list(16, 0).
	
get_init_rollback() ->
	utils:new_list(16, r).
	
start_tiles()->
	debug:debug_monitor("[Monitor]\t\t execute start_tiles()\n", []),
	process_flag(trap_exit, true),
	lists:foldr(fun(Id, Acc) -> 
		Pid = spawn_tile(Id),
		maps:put(Pid, Id, Acc) 
	end, #{}, get_tile_ids()).
	
spawn_tile(Id)->
	try 
		Pid = spawn_link(tile, start_tile,[Id]),
		glob:registerName(glob:regformat(Id), Pid),
		debug:debug_monitor("[Monitor]\t\t execute spawn_tile(~p)\n", [Id]),
		Pid
	catch 
		_:Reason -> 
			debug:debug_monitor("[Monitor]\t\t ~p in spawn_tile(~p)\n", [Reason, Id])
	end.
	
respawn_tile(Id, Value)->
	try 
		Pid = spawn_link(tile, restart_tile,[Id, Value]),
		glob:registerName(glob:regformat(Id), Pid),
		debug:debug_monitor("[Monitor]\t\t execute respawn_tile(~p, ~p)\n", [Id, Value]),
		Pid
	catch 
		_:Reason -> 
			debug:debug_monitor("[Monitor]\t\t ~p in respawn_tile(~p, ~p)\n", [Reason, Id, Value])
	end.

% when receiving the message $senddata, spaw a collector and a broadcaster for the collection of the data
%  from the tiles. Then, once the $Data is collected, inform the lifeguard and the gui
manageloop(State, Previous, Snapshot, Finished, TileMap, Rollback) ->
	receive
		{finished, Id, Lineshot}  ->
			RId = utils:get_nth(Id, Rollback),
			case RId of
				r ->
					debug:debug_manager("[Manager]\t\t received {finished, ~p, ~p}\n", [Id, Lineshot]),
					NFinished = maps:remove(Id, Finished),
					NSnapshot = lists:foldr(fun set_element/2, Snapshot, Lineshot),
					case (maps:size(NFinished) == 0) of
						true -> 
							lists:map(fun(I) ->
								send_message(I, new_turn)  
							end, get_tile_ids()),
							self() ! {collectedData, utils:equals_to_list(Previous, NSnapshot)},
							manageloop(passive, NSnapshot, NSnapshot, NFinished, TileMap, Rollback);
						false ->
							manageloop(State, Previous, NSnapshot, NFinished, TileMap, Rollback)
					end;
				_ ->
					debug:debug_manager("[Manager]\t\t received and rejected {finished, ~p, ~p}\n", [Id, Lineshot]),
					manageloop(State, Previous, Snapshot, Finished, TileMap, Rollback)
			end;
		up ->
			case State of
				passive ->
					debug:debug_manager("[Manager]\t\t received {up}\n", []),
					Tmp = [5,6,7,8],
					lists:map(fun(Id) -> 
						send_message(Id, up) 
					end, Tmp),
					manageloop(up, Previous, Snapshot, #{13=>[1,5,9,13],14=>[2,6,10,14],15=>[3,7,11,15],16=>[4,8,12,16]}, TileMap, Rollback);
				_ ->
					debug:debug_manager("[Manager]\t\t received and rejected {up}\n", []),
					manageloop(State, Previous, Snapshot, Finished, TileMap, Rollback)
			end;
		dn ->
			case State of
				passive ->
					debug:debug_manager("[Manager]\t\t received {dn}\n", []),
					Tmp = [9,10,11,12],
					lists:map(fun(Id) -> 
						send_message(Id, dn) 
					end, Tmp),
					manageloop(dn, Previous, Snapshot, #{1=>[13,9,5,1],2=>[14,10,6,2],3=>[15,11,7,3],4=>[16,12,8,4]}, TileMap, Rollback);
				_ ->
					debug:debug_manager("[Manager]\t\t received and rejected {dn}\n", []),
					manageloop(State, Previous, Snapshot, Finished, TileMap, Rollback)
			end;
		lx ->
			case State of
				passive ->
					debug:debug_manager("[Manager]\t\t received {lx}\n", []),
					Tmp = [2,6,10,14],
					lists:map(fun(Id) -> 
						send_message(Id, lx) 
					end, Tmp),
					manageloop(lx, Previous, Snapshot, #{4=>[1,2,3,4],8=>[5,6,7,8],12=>[9,10,11,12],16=>[13,14,15,16]}, TileMap, Rollback);
				_ ->
					debug:debug_manager("[Manager]\t\t received and rejected {lx}\n", []),
					manageloop(State, Previous, Snapshot, Finished, TileMap, Rollback)
			end;
		rx ->
			case State of
				passive ->
					debug:debug_manager("[Manager]\t\t received {rx}\n", []),
					Tmp = [3,7,11,15],
					lists:map(fun(Id) -> 
						send_message(Id, rx) 
					end, Tmp),
					manageloop(rx, Previous, Snapshot, #{1=>[4,3,2,1],5=>[8,7,6,5],9=>[12,11,10,9],13=>[16,15,14,13]}, TileMap, Rollback);
				_ ->
					debug:debug_manager("[Manager]\t\t received and rejected {rx}\n", []),
					manageloop(State, Previous, Snapshot, Finished, TileMap, Rollback)
			end;
		sendData ->
			debug:debug_manager("[Manager]\t\t received {sendData}\n", []),
			manageloop(State, Previous, Snapshot, Finished, TileMap, Rollback);
		{collectedData, R} ->
			debug:debug_manager("[Manager]\t\t collectedData {collectedData, ~p}\n", [R]),
			case R of
				false ->
					NSnapshot = randomiseatile(Snapshot),
					gui ! {values, NSnapshot},
					manageloop(State, NSnapshot, NSnapshot, Finished, TileMap, Rollback);
				true ->
					gui ! {values, Snapshot},
					manageloop(State, Snapshot, Snapshot, Finished, TileMap, Rollback)
			end;
		{'EXIT', Pid, _} ->
			debug:debug_monitor("[Monitor]\t\t received {killed, ~p} \n", [Pid]),
			Id = maps:get(Pid, TileMap, "Default value"),
			Value = utils:get_nth(Id, Snapshot),
			NTileMap = maps:remove(Pid, TileMap),
			NPid = respawn_tile(Id, Value),
			NNTileMap = maps:put(NPid, Id, NTileMap),

			case State of
				passive ->
					manageloop(State, Previous, Snapshot, Finished, NNTileMap, Rollback);
				_ -> 
					List = get_line(Id, Finished),
					case (utils:size_of_list(List) == 0) of
						true ->
							manageloop(State, Previous, Snapshot, Finished, NNTileMap, Rollback);
						false ->
							NRollback = lists:foldr(fun(I, Acc) -> 
								case (Id /= I) of
									true ->
										R = utils:get_nth(I, Acc),
										case R of
											r -> 
												send_message(I, {rc, utils:get_nth(I, Snapshot)}),
												utils:set_nth(I, Acc, rc);
											_ -> 
												Acc
										end;
									false -> 
										send_message(I, {rc, utils:get_nth(I, Snapshot)}),
										utils:set_nth(I, Acc, rc)
								end
							end, Rollback, List),
							manageloop(State, Previous, Snapshot, Finished, NNTileMap, NRollback)
					end
			end;
		{ra, Id} ->
			debug:debug_monitor("[Monitor]\t\t received {ra, ~p} \n", [Id]),
			NRollback = utils:set_nth(Id, Rollback, ra),
			[A, B, C, D] = get_line(Id, Finished),
			Restart = lists:foldr(fun(I, Bool) ->
						case Bool of
							true ->
								R = utils:get_nth(I, NRollback),
								case R of 
									ra -> true;
									_ -> false
								end;
							false -> 
								false
						end
					end, true, [A, B, C, D]),
			case Restart of
				true ->
					NNRollback = lists:foldr(fun(I, Acc) -> utils:set_nth(I, Acc, r) end, NRollback, [A, B, C, D]),
					lists:map(fun(I) -> send_message(I, ro) end, [A, B, C, D]),
					send_message(B, State),
					manageloop(State, Previous, Snapshot, Finished, TileMap, NNRollback);
				false ->
					manageloop(State, Previous, Snapshot, Finished, TileMap, NRollback)
			end
	end.
	
get_line(Id, Finished)->
	lists:foldr(
		fun(List, Acc) -> 
			case lists:member(Id, List) of
				true -> List;
				false -> Acc
			end
		end, [], maps:values(Finished)).
	
	
set_element({Id, Value}, Snapshot) ->
	utils:set_nth(Id, Snapshot, Value).

send_message(Id2, Message) ->
	try
		glob:regformat(Id2) ! Message,
		debug:debug_manager("[Manager]\t\t send ~p to ~p\n", [Message, Id2])
	catch 
		_:Reason -> 
			debug:debug_manager("[Manager]\t\t ~p in send ~p to ~p\n", [Reason, Message, Id2])
	end.

send_setvalue(Id2, Value, Merged)->	
	send_message(Id2, {setvalue, Value, Merged}).
	
zeroes_in_snapshot(Snapshot)->
	utils:size_of_list(lists:filter(fun(Value) -> (Value == 0) end, Snapshot)).

randomiseatile(Snapshot)->
	{A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
	case zeroes_in_snapshot(Snapshot) of
		0 ->
			Snapshot;
		_ ->
			Id = getCand(0, Snapshot), Value = getValue(),
			debug:debug_manager("[Manager]\t\t: radomised in ~p.~n",[Value]),
			NSnapshot = utils:set_nth(Id, Snapshot, Value),
			send_setvalue(Id, Value, false),
			NSnapshot
	end.

getValue()->
	C = random:uniform(8),
	case (C =< 6) of
		true -> 2;
		false -> 4
	end.

getCand(Oth , Snapshot)->
	C = random:uniform(16),
	case C of
		Oth -> getCand(Oth, Snapshot);
		_ ->
			case utils:get_nth(C, Snapshot) of
				0 -> C;
				_ -> getCand(Oth, Snapshot)
			end
	end.
