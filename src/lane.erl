-module(lane).

-export([
	loop/0,
	swimmer/3,
	start/2,
	config/2
	]).

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
			case Cap =:= 0 of
				true ->
					Sender ! {self(), 0};
				false ->
					Sender ! {self(), {avialable, Cap}}
			end;
		{Sender, {request, state}} ->
			Cap = get(capacity),
			ID = get(id),
			Sender ! {self(), {status, ID, Cap}};
		{Sender, {swim, Time}} ->
			spawn(?MODULE, swimmer, [self(), Time, get(id)]),
			NewCap = get(capacity) - 1,
			put(capacity, NewCap),
			Sender ! {get(id), {swimmer_entered}};
		{_ , {swimmer, finished}} ->
			io:format("\n \nSwimmer finished swimming on lane ID: ~w\n \n", [get(id)]),
			NewCap = get(capacity) + 1,
			put(capacity, NewCap);
		{terminate} ->
			exit(user_exit);
		_ ->
			c:flush(), []
	end,
	loop().

swimmer(Parent, Time, ID) ->
	io:format("\nSwimmer entering lane ~w\n", [ID]),
	timer:send_after(Time, Parent, {self(), {swimmer, finished}}).
