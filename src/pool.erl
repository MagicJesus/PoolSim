-module(pool).
-behaviour(gen_server).

-compile(export_all).

-define(DefaultSwimmingLanes, 4).
-define(DefaultCapacity, 4).

% -- funkcja do 'wchodzenia do basenu'
swim(Time) when is_integer(Time) -> 
	gen_server:call(?MODULE, {swim, Time}). % gen_server:call wywołuje moja metode handle_call/3

% -- funkcja start() odpala serwer i tworzy odpowiednia ilosc torów
start() ->
	start({?DefaultSwimmingLanes, ?DefaultCapacity}).
start({NumOfLanes, Capacity}) 	when is_integer(NumOfLanes)
								andalso is_integer(Capacity) ->
	Number = max(NumOfLanes, 1),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Number, Capacity], []);
start(_) ->
	message({invalid_args, "start()"}).

% -- init() jest wolany przy wywolaniu gen_server:start_link()
init([SwimmingLanes, Capacity]) ->
    process_flag(trap_exit, true),
    {ok, #{ lane_pids => [ lane:start(ID, Capacity) || ID <- lists:seq(1, SwimmingLanes) ]}}.

% -- pomocnicza funkcja do informowania o bledzie	
message({invalid_args, FunctionName}) ->
    FunctionName ++ " > Invalid Argument(s).".
	
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
	end.
	
handle_cast(_Request, State) -> 
	{noreply, State}.

handle_info(_Request, State) -> 
	{noreply, State}.

terminate(_Reason, _State) -> 
	ok.

code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.

lane_processes(State) ->
    maps:get(lane_pids, State, []).

check_for_first_free_lane(PIDs) ->
	[PID ! {self(), {request, available}} || PID <- PIDs], % wyslanie prosby o informacje o dostepnosci toru	
	Response = collect(PIDs),
	Filtered = lists:filter(fun(X) -> not_zero(X) end, Response),
	% ta funkcja bedzie zwracac id pierwszego wolnego napotkanego toru
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
        {PID, {avialable, _}} ->
            [ PID | collect(PIDs -- [PID])];
        {PID, Response } ->
            [ Response | collect(PIDs -- [PID])];
        _ -> c:flush(), [] % Fail quietly and flush message queue.
    end.

not_zero(X) ->
	case X =:= 0 of
		true ->
			false;
		false ->
			true
	end.