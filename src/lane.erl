-module(lane).

-compile(export_all).

start(ID, Capacity) ->
	spawn(?MODULE, config, [ID, Capacity]).
	

config(ID, Capacity) ->
	put(id, ID),
	put(capacity, Capacity),
	loop().
	
loop() ->
	receive
		{Sender, _} ->
			Sender ! io:format("ID: ~w Capacity: ~w\n", [get(id), get(capacity)])
	end,
	loop().