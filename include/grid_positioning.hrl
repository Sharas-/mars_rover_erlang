%%%--------------------------------------------------------------------- 
%%% @doc definitions for grid position tracking
%%%---------------------------------------------------------------------
-type coords():: {pos_integer(), pos_integer()}.
-type grid_def():: coords().
-type direction():: east | west | north | south.
-type position():: {coords(), direction()}.