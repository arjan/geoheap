
-record(geosample,
        {lat,
         lon,
         time,
         text}).


-define(DEBUG(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var])).
