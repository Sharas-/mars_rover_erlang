-module(position_tracking_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

commands_LF_test()->
 	?assertEqual({{99,0}, west}, position_tracking:commands_to_position("LF", {100, 100}, {{0,0}, south})).

 commands_FFRFFLLB_test()->
 	?assertEqual({{3,2}, west}, position_tracking:commands_to_position("FFRFFLLB", {100, 100}, {{0,0}, south})).

 commands_BBRFLFRBB_test()->
  	?assertEqual({{99,99}, east}, position_tracking:commands_to_position("BBRFLFRBB", {100, 100}, {{0,0}, south})).

 commands_LLFFFRBBB_test()->
   	?assertEqual({{3,97}, west}, position_tracking:commands_to_position("LLFFFRBBB", {100, 100}, {{0,0}, south})).