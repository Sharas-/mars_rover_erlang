-module(rover_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

move_FBLLFFRF_test()->
	rover:init(fun(_) -> false end),
	?assertEqual({ack, "FBLLFFRF"}, rover:execute_moves("FBLLFFRF")).

collision_after_FLFRRB_test()->
	rover:init(fun(Path) -> Path == "FLFRRB" end),
	?assertEqual({nack, "FLFRRB"}, rover:execute_moves("FLFRRBFFF")).

collision_after_FBLLFFRF_test()->
	rover:init(fun(Path) -> Path == "FBLLFFRF" end),
	?assertEqual({nack, "FBLLFFRF"}, rover:execute_moves("FBLLFFRFLFRRB")).