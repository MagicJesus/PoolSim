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
			io:format("Capacity while requesting: ~w\n", [Cap]),
			case Cap =:= 0 of
				true -> 
					Sender ! noop;
				false ->
					Sender ! {self(), Cap}
			end;
		{Sender, {swim, Time}} ->
			spawn(?MODULE, swimmer, [self(), Time]),
			NewCap = get(capacity) - 1,
			put(capacity, NewCap),
			Sender ! {get(id), {swimmer_entered}};
		{_ , {swimmer, finished}} ->
			io:format("Swimmer finished... lane ID: ~w, capacity before leaving: ~w\n", [get(id), get(capacity)]),
			NewCap = get(capacity) + 1,
			put(capacity, NewCap);
		{Sender, _} ->
			Sender ! io:format("ID: ~w Capacity: ~w\n", [get(id), get(capacity)])
	end,
	loop().

swimmer(Parent, Time) ->
	io:format("Swimmer entering...\n"),
	timer:send_after(Time, Parent, {self(), {swimmer, finished}}).