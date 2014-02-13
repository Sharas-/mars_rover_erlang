-module(rover).
-export([init/1, execute_moves/1]).

%%----------------------------------------------------------------------
%% Purpose:	Initializes rover
%% Args:	CollisionDetector - function which checks for collisions on the path
%% Returns:	ok
%%----------------------------------------------------------------------
-spec init(fun((list()) -> boolean())) -> ok.
init(CollisionDetector) ->
	erlang:put(collision_detector, CollisionDetector),
	ok.

%%----------------------------------------------------------------------
%% Purpose:	Commands rover to execute movements
%% Args:	Movements - string of movement representing letters
%% Returns:	{ack, Movements} if all movements were executed successfuly, {nack, MovementsExecuted} if collision was detected on the way
%%----------------------------------------------------------------------
-spec execute_moves(nonempty_list()) -> {ack|nack, list()}.
execute_moves(Movements) ->
	execute_moves(Movements, [], erlang:get(collision_detector)).

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

 %--- private ---

move($F)->
	forward_ok;
move($B)->
	back_ok;
move($L)->
	left_ok;
move($R)->
	right_ok;
move(_) ->
	unknown_command.