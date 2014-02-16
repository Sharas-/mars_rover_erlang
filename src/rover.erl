%%%--------------------------------------------------------------------- 
%%% @doc Rover implementation. 
%%% Rover only executes movement commands. It is stateless and does not track its own position.
%%%--------------------------------------------------------------------- 
-module(rover).
-behaviour(gen_server).

-export([start/1, execute_moves/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type collision_detector() :: fun((list()) -> boolean()).


%%----------------------------------------------------------------------
%% Purpose:	Starts rover process
%% Args:	CollisionDetector - function which checks for collisions on the path
%% Returns:	{ok, pid()}
%%----------------------------------------------------------------------
-spec start(collision_detector()) -> {ok, pid()}.
start(CollisionDetector) ->
	gen_server:start_link(?MODULE, CollisionDetector, []).

%%----------------------------------------------------------------------
%% Purpose:	Commands rover to execute movements
%% Args:	Movements - string of movement representing letters
%% Returns:	{ack, Movements} if all movements were executed successfuly, {nack, MovementsExecuted} if collision was detected on the way
%%----------------------------------------------------------------------
-spec execute_moves(nonempty_list(), pid()) -> {ack|nack, list()}.
execute_moves(Movements, Pid) ->
	gen_server:call(Pid, {execute_moves, Movements}).

%%----------------------------------------------------------------------
%% Purpose:	Stops rover process
%% Returns:	ok
%%----------------------------------------------------------------------
-spec stop(pid()) -> ok.
stop(Pid)->
	gen_server:cast(Pid, stop).

%--- gen_server callbacks ---

-spec init(collision_detector()) -> {ok, collision_detector()}.
init(CollisionDetector) ->
	{ok, CollisionDetector}.

-spec handle_call({execute_moves, nonempty_list()}, {pid(), term()}, collision_detector()) -> {reply, {ack|nack, list()}}.
handle_call({execute_moves, Moves}, _From, CollisionDetector) ->
	{reply, execute_moves(Moves, [], CollisionDetector), CollisionDetector}.

-spec handle_cast(stop, collision_detector()) -> {stop, normal, collision_detector()}.
handle_cast(stop, CollisionDetector) ->
	{stop, normal, CollisionDetector}.

-spec handle_info(term(), collision_detector()) -> {noreply, collision_detector()}.
handle_info(_, State) ->
	{noreply, State}.

-spec terminate(term(), collision_detector()) -> ok.
terminate(_Reason, _State) ->
	ok.

-spec code_change(term(), collision_detector(), term()) -> {ok, collision_detector()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%--- private ---

execute_moves([CurrMovement | Remaining], Moved, Collision_detector) ->
	case Collision_detector(lists:reverse(Moved)) of
		true ->	
			{nack, lists:reverse(Moved)};
		false -> 
			move(CurrMovement),
			execute_moves(Remaining, [CurrMovement|Moved], Collision_detector)
	end;
execute_moves([], Moved, _) ->
	{ack, lists:reverse(Moved)}.


move($F)->
	forward_ok;
move($B)->
	back_ok;
move($L)->
	left_ok;
move($R)->
	right_ok.