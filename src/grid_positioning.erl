%%%--------------------------------------------------------------------- 
%%% @doc fnctions to calculate objects' position on a grid
%%%--------------------------------------------------------------------- 

-module(grid_positioning).
-export([move_forward/2, move_back/2, turn_left/1, turn_right/1]).

-type coords():: {pos_integer(), pos_integer()}.
-type grid_def():: coords().
-type direction():: east | west | north | south.
-type position():: {coords(), direction()}.

%%----------------------------------------------------------------------
%% Purpose:	Calculates objets' position after it moves forward
%% Args:	GridDef - definition of the grid object is moving on
%%          Position - current position of the object
%% Returns:	New position of the object
%%----------------------------------------------------------------------
-spec move_forward(grid_def(), position()) -> position().
move_forward({GridX, GridY}, {X, Y, Direction}) ->
	move({GridX, GridY}, {X, Y, Direction}, 1).

%%----------------------------------------------------------------------
%% Purpose:	Calculates objets' position after it moves back
%% Args:	GridDef - definition of the grid object is moving on
%%          Position - current position of the object
%% Returns:	New position of the object
%%----------------------------------------------------------------------
-spec move_back(grid_def(), position()) -> position().
move_back({GridX, GridY}, {X, Y, Direction}) ->
	move({GridX, GridY}, {X, Y, Direction}, -1).

%%----------------------------------------------------------------------
%% Purpose:	Calculates objets' direction after it turns left
%%          Direction - current direction the object is facing
%% Returns:	New direction of the object
%%----------------------------------------------------------------------
-spec turn_left(direction()) -> direction().
turn_left(Direction) ->
	{Lturn, _} = turn(Direction),
	Lturn.
%%----------------------------------------------------------------------
%% Purpose:	Calculates objets' direction after it turns right
%%          Direction - current direction the object is facing
%% Returns:	New direction of the object
%%----------------------------------------------------------------------
-spec turn_right(direction()) -> direction().
turn_right(Direction) ->
	{_, Rturn} = turn(Direction),
	Rturn.

%--------------- private functions ---------------------------------

-spec move(grid_def(), position(), 1|-1) -> position().
move({GridX, GridY}, {X, Y, Direction}, Dir) ->
	{wrap(X + (Dir * forward_x_change(Direction)), GridX), wrap(Y + (Dir * forward_y_change(Direction)), GridY)}.

%%----------------------------------------------------------------------
%% Purpose:	Calculates how forward move affects Y coordinate
%% Args:	Direction - direction object is facing
%% Returns:	Change of Y coordinate
%%----------------------------------------------------------------------
-spec forward_y_change(direction()) -> integer(). 
forward_y_change(north) ->
	-1;
forward_y_change(south) ->
	1;
forward_y_change(_) ->
	0.
%%----------------------------------------------------------------------
%% Purpose:	Calculates how forward move affects X coordinate
%% Args:	Direction - direction object is facing
%% Returns:	Change of X coordinate
%%----------------------------------------------------------------------
-spec forward_x_change(direction()) -> integer().
forward_x_change(east) ->
	1;
forward_x_change(west) ->
	-1;
forward_x_change(_) ->
	0.
%%----------------------------------------------------------------------
%% Purpose:	Calculates objets' direction after it turns left or right
%% Args:	Direction - current direction the object is facing
%% Returns:	Tuple of directions. First is the direction object is facing after left turn, second - after right turn.
%%----------------------------------------------------------------------
-spec turn(direction()) -> {direction(), direction()}.
turn(east) ->
	{south, north};
turn(west) ->
	{north, south};
turn(south) ->
	{west, east};
turn(north) ->
	{east, west}.
%%----------------------------------------------------------------------
%% Purpose:	"Circles" a value within [0, MaxVal] range
%% Args:	Val - value to be "circled"
%% Returns:	Value "cicled back" if Val is negative, othervise - value "circled forward"
%%----------------------------------------------------------------------
-spec wrap(integer(), pos_integer()) -> pos_integer().
wrap(Val, MaxVal) when Val < 0 ->
	MaxVal + (Val rem MaxVal);
wrap(Val, MaxVal) ->
	Val rem MaxVal.