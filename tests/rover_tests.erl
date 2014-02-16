-module(rover_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

move_FBLLFFRF_test()->
	{ok, Pid} = rover:start(fun(_) -> false end),
	?assertEqual({ack, "FBLLFFRF"}, rover:execute_moves("FBLLFFRF", Pid)),
	rover:stop(Pid).

collision_after_FLFRRB_test()->
	{ok, Pid} = rover:start(fun(Path) -> Path == "FLFRRB" end),
	?assertEqual({nack, "FLFRRB"}, rover:execute_moves("FLFRRBFFF", Pid)),
	rover:stop(Pid).

collision_after_FBLLFFRF_test()->
	{ok, Pid} = rover:start(fun(Path) -> Path == "FBLLFFRF" end),
	?assertEqual({nack, "FBLLFFRF"}, rover:execute_moves("FBLLFFRFLFRRB", Pid)),
	rover:stop(Pid).