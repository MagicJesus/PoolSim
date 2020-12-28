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
start({NumOfLanes, Capacity}) when is_integer(NumOfLanes) 
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
	[PID ! {self(), id} || PID <- PIDs ],
	{reply, PIDs, State}.
	
handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

lane_processes(State) ->
    maps:get(lane_pids, State, []).
	
	
