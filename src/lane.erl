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
		{Sender, {request, available}} ->
			Cap = get(capacity),
			% io:format("Capacity while requesting: ~w\n", [Cap]),
			case Cap =:= 0 of
				true -> 
					Sender ! {self(), unavialable};
				false ->
					Sender ! {self(), {avialable, Cap}}
			end;
		{Sender, {swim, Time}} ->
			spawn(?MODULE, swimmer, [self(), Time, get(id)]),
			NewCap = get(capacity) - 1,
			put(capacity, NewCap),
			Sender ! {get(id), {swimmer_entered}};
		{_ , {swimmer, finished}} ->
			io:format("Swimmer finished swimming on lane ID: ~w\n", [get(id)]),
			NewCap = get(capacity) + 1,
			put(capacity, NewCap);
		{Sender, _} ->
			Sender ! io:format("ID: ~w Capacity: ~w\n", [get(id), get(capacity)])
	end,
	loop().

swimmer(Parent, Time, ID) ->
	io:format("Swimmer entering lane ~w\n", [ID]),
	timer:send_after(Time, Parent, {self(), {swimmer, finished}}).