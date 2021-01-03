-module(pool).
-behaviour(gen_server).

-export([
	menu/0,
	swim/0,
	swim/1,
	status/0,
	start/0,
	start/1,
	stop/0,
	message/1,
	lane_processes/1,
	check_for_first_free_lane/1,
	collect/1,
	not_zero/1,
	put_list/1,
	get_input/0
	]).
-export([
	init/1,
	handle_cast/2,
	handle_call/3,
	handle_info/2,
	terminate/2,
	code_change/3
	]).

-define(DefaultSwimmingLanes, 4).
-define(DefaultCapacity, 4).

% -- funkcja do obslugiwania programu
menu() ->
	io:format("\n## WELCOME TO THE SIMPLEST POOL SIMULATOR ##\n"),
	io:format("   What would you like to do? \n"),
	io:format("1: Check the pool status \n"),
	io:format("2: Enter the pool for desired amount of time (in seconds)\n"),
	io:format("3: Exit \n"),
	{ok, UserInput} = io:read("Your choice: "),
	case UserInput of
		1 ->
			io:format("\n\n---------STATUS CHECK---------\n\n"),
			status(),
			io:format("\n\n"),
			menu();
		2 ->
			{ok, Time} = io:read("Enter the amount of time: "),
			swim(Time * 1000),
			timer:sleep(100),
			menu();
		3 ->
			stop();
		_ ->
			io:format("Undefined Command, please enter 1, 2 or 3\n\n"),
			menu()
	end.
% -- funkcja do 'wchodzenia do basenu'
swim(Time) when is_integer(Time) ->
	gen_server:call(?MODULE, {swim, Time}); % gen_server:call wywołuje moja metode handle_call/3
swim(_) ->
	message({invalid_args, "swim()"}).
swim() ->
	message({no_args_given, "swim()", "1"}).
% -- funkcja do sprawdzania aktualnego stanu torów
status() ->
	gen_server:call(?MODULE, {check_status}).

% -- funkcja start() odpala serwer i tworzy odpowiednia ilosc torów
start() ->
	io:format("Would you like to change the number of lanes and their capacity (default 4/4)? (y/n)\n"),
	get_input().
start({NumOfLanes, Capacity}) 	when is_integer(NumOfLanes)
								andalso is_integer(Capacity) ->
	Number = max(NumOfLanes, 1),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Number, Capacity], []),
	menu();
start(_) ->
	message({invalid_args, "start()"}).
% -- funkcja do zatrzymania i zamknięcia basenu
stop() ->
	gen_server:stop(?MODULE).

% -- init() jest wolany przy wywolaniu gen_server:start_link()
init([SwimmingLanes, Capacity]) ->
    process_flag(trap_exit, true),
    {ok, #{ lane_pids => [ lane:start(ID, Capacity) || ID <- lists:seq(1, SwimmingLanes) ]}}.

% -- pomocnicza funkcja do informowania o bledach
message({invalid_args, FunctionName}) ->
    FunctionName ++ " > Invalid Argument(s).";
message({no_args_given, FunctionName, ArgNum}) ->
	FunctionName ++ " must have at at least " ++ ArgNum ++ " argument(s)".

% -- handle_call/3 to callback do gen_server:call
handle_call({swim, Time}, _From, State) ->
	PIDs = lane_processes(State),
	Lane = check_for_first_free_lane(PIDs),
	case Lane =:= 0 of
		true ->
			io:format("No free lanes"),
			{reply, {all_lanes_taken}, State};
		false ->
			Lane ! {self(), {swim, Time}},
			{reply, {ok, Lane}, State}
	end;
handle_call({check_status}, _From, State) ->
	PIDs = lane_processes(State),
	[PID ! {self(), {request, state}} || PID <- PIDs],
	Response = collect(PIDs),
	put_list(Response),
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Request, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	io:format("User Exit \n"),
	PIDs = lane_processes(State),
	[PID ! {terminate} || PID <- PIDs],
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
% -- funkcja pomocnicza, pobiera PIDy procesów torów pływackich
lane_processes(State) ->
    maps:get(lane_pids, State, []).

% -- funkcja zwraca PID pierwszego wolnego toru
check_for_first_free_lane(PIDs) ->
	[PID ! {self(), {request, available}} || PID <- PIDs], % wyslanie prosby o informacje o dostepnosci toru
	Response = collect(PIDs),
	Filtered = lists:filter(fun(X) -> not_zero(X) end, Response),
	case length(Filtered) =:= 0 of
        true -> 0;
        false ->
            [H|_] = Filtered,
			  H
    end.

% funkcja collect służy do odbierania komunikatów od torów
collect([]) -> [];
collect(PIDs) ->
    receive
		{PID, {status, ID, Cap}} ->
			[ {ID, Cap} | collect(PIDs -- [PID])];
		{PID, {avialable, _}} ->
      		[ PID | collect(PIDs -- [PID])];
    	{PID, Response } ->
      		[ Response | collect(PIDs -- [PID])];
    	_ -> c:flush(), [] % Fail quietly and flush message queue.
    end.
% -- funkcja pomocnicza do wyodrebniania PID-ów torów pływackich z listy
not_zero(X) ->
	case X =:= 0 of
		true ->
			false;
		false ->
			true
	end.

% funkcja pomocnicza do wypisywania danych o stanie torów
put_list([]) ->
	io:format("");
put_list([{ID,Cap}|Tail]) ->
	io:format("Current capacity on lane ~w is : ~w\n", [ID, Cap]),
	put_list(Tail).
% -- funkcja do pobierania danych od uzytkownika
get_input() ->
	{ok, UserInput} = io:read(""),
	case UserInput of
		y ->
			{ok, UserLanes} = io:read("Enter the desired number of lanes: "),
			{ok, UserCapacity} = io:read("Enter the desired lane capacity: "),
			start({UserLanes, UserCapacity});
		n ->
			start({?DefaultSwimmingLanes, ?DefaultCapacity});
		_ ->
			io:format("Invalid input, please enter a single character (y or n)\n"),
			get_input()
	end.
