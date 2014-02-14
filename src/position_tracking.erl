%%%--------------------------------------------------------------------- 
%%% @doc Movement commands translation to position functions.
%%%--------------------------------------------------------------------- 
-module(position_tracking).
-export([commands_to_position/3]).
-include("include/grid_positioning.hrl").

%%----------------------------------------------------------------------
%% Purpose:	Calculates objects' position based on a list of movement commands
%% Args:	Commands - List of movement commands
%%			GridDef - Definition of the grid object is moving on
%%			Position - Initial position of the object
%% Returns:	Position of the object after all movements are executed
%%----------------------------------------------------------------------
-spec commands_to_position(list(), grid_def(), position()) -> position().
commands_to_position([CurrCommand | Remaining], GridDef, CurrPosition)->
	NewPos = execute(CurrCommand, GridDef, CurrPosition),
	commands_to_position(Remaining, GridDef, NewPos);
commands_to_position([], _, CurrPosition) ->
	CurrPosition.

%--- private ---

execute($F, GridDef, CurrPosition)->
	grid_positioning:move_forward(GridDef, CurrPosition);
execute($B, GridDef, CurrPosition)->
	grid_positioning:move_back(GridDef, CurrPosition);
execute($L, _, {Coords, Direction})->
	{Coords, grid_positioning:turn_left(Direction)};
execute($R, _, {Coords, Direction})->
	{Coords, grid_positioning:turn_right(Direction)}.