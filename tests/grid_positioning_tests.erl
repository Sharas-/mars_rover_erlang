-module(grid_positioning_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(GRID_DEF, {100, 100}).

move(Where, Direction) ->
	grid_positioning:Where(?GRID_DEF, {{50, 50}, Direction}).

move(Where, Direction, Coords) ->
	grid_positioning:Where(?GRID_DEF, {Coords, Direction}).

turn(Where, Direction) ->
	grid_positioning:Where(Direction).

%--- move tests ---

move_forward_south_test() ->
	?assertEqual({{50, 51}, south}, move(move_forward, south)).

move_forward_north_test() ->
	?assertEqual({{50, 49}, north}, move(move_forward, north)).

move_forward_west_test() ->
	?assertEqual({{49, 50}, west}, move(move_forward, west)).

move_forward_east_test() ->
	?assertEqual({{51, 50}, east}, move(move_forward, east)).

move_back_south_test() ->
	?assertEqual({{50, 49}, south}, move(move_back, south)).

move_back_north_test() ->
	?assertEqual({{50, 51}, north}, move(move_back, north)).

move_back_west_test() ->
	?assertEqual({{51, 50}, west}, move(move_back, west)).

move_back_east_test() ->
	?assertEqual({{49, 50}, east}, move(move_back, east)).

%--- direction tests ---

turn_left_east_test() ->
	?assertEqual(south, turn(turn_left , east)).

turn_left_west_test() ->
	?assertEqual(north, turn(turn_left , west)).

turn_left_north_test() ->
	?assertEqual(east, turn(turn_left , north)).

turn_left_south_test() ->
	?assertEqual(west, turn(turn_left , south)).

turn_right_east_test() ->
	?assertEqual(north, turn(turn_right , east)).

turn_right_west_test() ->
	?assertEqual(south, turn(turn_right , west)).

turn_right_north_test() ->
	?assertEqual(west, turn(turn_right , north)).

turn_right_south_test() ->
	?assertEqual(east, turn(turn_right , south)).

%--- wrap tests ---

wrap_x_left_test() ->
	?assertEqual({{99, 0}, west}, move(move_forward, west, {0, 0})).

wrap_x_right_test() ->
	?assertEqual({{0, 0}, east}, move(move_forward, east, {99, 0})).

wrap_y_down_test() ->
	?assertEqual({{0, 99}, north},  move(move_forward, north, {0, 0})).

wrap_y_up_test() ->
	?assertEqual({{0, 0}, south}, move(move_forward, south, {0, 99})).